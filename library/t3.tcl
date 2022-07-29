# https://github.com/okhlybov/t3

# ... and now the True Test (tm)
#     Hold fast or expire ...

package provide T3 0.1.0

if {$tcl_interactive} {error "T3 package can't be used from within the interactive session"}

package require Tcl 8.6
package require Itcl
package require Thread

if {[catch {set @boxed}]} {

  # The code below is executed by the master interpreter

  # https://stackoverflow.com/questions/57348896/crc16-calculation-in-tcl
  proc crc16ish {str} { 
    set Polynom 0x1021
    set Highbit 0x8000
    set MASK 0xFFFF
    set rem 0xFFFF
    binary scan $str c* bytes
    foreach byte $bytes {
      set rem [expr {$rem ^ ($byte << 8)}]
      foreach _ {7 6 5 4 3 2 1 0} {
        set rem [expr {(($rem << 1) ^ ($rem & $Highbit ? $Polynom : 0)) & $MASK}]
      }
    }
    return $rem
  }

  # https://wiki.tcl-lang.org/page/lshift
  proc lshift {args} {
    lassign $args listVar count
    upvar 1 $listVar var
    if {![info exists var]} {
      return -level 1 -code error [subst {can't read "$listVar": no such variable}]
    }
    switch -exact -- [llength $args] {
      1 {
          set var [lassign $var value]
      }
      2 {
          set value [lrange $var 0 $count-1]
          set var [lrange $var $count end]
      }
      default {
        return -level 1 -code error [subst {wrong # args: should be "lshift listVar ?count?"}]
      }
    }
    return $value
  }

  # Ordered set operations on sorted lists of unique elements
  namespace eval set {
    # {*}
    proc create {args} {
      return [lsort -unique $args]
    }
    # set1 | set2
    proc join {set1 set2} {
      return [create {*}$set1 {*}$set2]
    }
    # set1 - set2
    proc subtract {set1 set2} {
      foreach e $set2 {set set1 [lsearch -sorted -inline -exact -all -not $set1 $e]}
      return $set1
    }
    # set1 & set2
    proc intersect {set1 set2} {
      set s [list]
      foreach e [join $set1 $set2] {
        if {[lsearch -sorted -all $set1 $e] >= 0 && [lsearch -sorted -all $set2 $e] >= 0} {lappend s $e}
      }
      return [create {*}$s]
    }
    # set1 ^ set2
    proc disjoin {set1 set2} {
      return [subtract [join $set1 $set2] [intersect $set1 $set2]]
    }
    # True if set1 and set2 are disjoint i.e. share no common elements
    proc disjoint? {set1 set2} {
      return [expr {[llength [intersect $set1 $set2]] == 0}]
    }
    # True if subset is a proper subset of set
    proc subset? {set subset} {
      return [expr {[intersect $set $subset] eq $subset}]
    }
  }

  # Async unit
  itcl::class unit {

    # Unit unique index
    private variable index

    # Unit name
    public variable name {}

    # Set of assigned tags
    public variable tags [list]

    # Setup code block
    public variable setup {}

    # Cleanup code block
    public variable cleanup {}

    # Main code block
    public variable code

    # List of skip reasons
    private variable skips [list]

    constructor {code} {
      incr count
      set index $count
      configure -code $code
    }

    # Construct a quasi-unique @tag for unit
    method quid {} {
      return @[format %04x [crc16ish "$name$index[pwd]$::argv0"]]
    }

    # Submit unit's job
    method submit {} {
      if {[llength $::quids]} {
        # Force run specific units, disable tags filtering
        if {[lsearch $::quids [quid]] < 0} {return}
      } else {
        # Default unit filtering rules apply
        if {[llength $tags]} {
          # List of skip reasons
          if {[llength $::tags::none] && ![set::disjoint? $tags $::tags::none]} {lappend skips none}
          if {[llength $::tags::any] && [set::disjoint? $tags $::tags::any]} {lappend skips any}
          if {[llength $::tags::all] && ![set::subset? $tags $::tags::all]} {lappend skips all}
          if {[llength $skips]} {
            lappend skipped [self]
            return
          }
        }
      }
      lappend pending [tpool::post $pool [script]]
      lappend submitted [self]
    }

    # Render the job's script
    method script {} {
      subst -nocommands {
        @stdout reset
        @stderr reset
        set outcome [catch {
          $setup
          try {
            $code
          } finally {
            $cleanup
          }
        } result options]
        flush stdout
        flush stderr
        return [dict merge \$options [dict create -this $this -stdout [@stdout stream] -stderr [@stderr stream] -result \$result]]
      }
    }

    # Chop trailing empty lines from the list
    private proc chop-trail {list} {
      set empty 1
      set x [llength $list]; incr x -1
      for {set i [expr {[llength $list]-1}]} {$i >= 0} {incr i -1} {
        if {[string trim [lindex $list $i]] ne ""} {
          set empty 0
          break
        }
      }
      if {!$empty} {return [lrange $list 0 $i]} else {return [list]}
    }

    private method full-name {} {
      if {$name eq ""} {
        return [quid]
      } else {
        return "$name [quid]"
      }
    }

    private proc puts-yaml-block {pad tag report} {
      set lines [chop-trail [split $report "\n"]]
      if {[lindex $lines end] eq ""} {set lines [lrange $lines 0 end-1]}
      switch [llength $lines] {
        0 {}
        1 {puts "$pad$tag: [lindex $lines 0]"}
        default {
          puts "$pad$tag: |"
          foreach line $lines {puts "${pad}  $line"}
        }
      }
    }

    private proc puts-markdown-block {tag report} {
      set lines [chop-trail [split $report "\n"]]
      switch [llength $lines] {
        0 {}
        1 {puts "- $tag: [lindex $lines 0]"}
        default {
          puts "- $tag:"
          puts "    ~~~~"
          foreach line $lines {puts "    $line"}
          puts "    ~~~~"
        }
      }
    }

    # Process the completed job's outcome
    method process-completed {outcome} {
      # Passed units' details are output in verbose mode only
      dict with outcome {
        if {$::progress && !$::quiet} {
          switch ${-code} {
            0 {puts -nonewline stderr .}
            default {puts -nonewline stderr F}
          }
          flush stderr
        }
        switch $::report {
          tap {
            if {${-code}} {
              puts "\n${::pad}not ok $index - [full-name] # ${-result}"
            } else {
              puts "\n${::pad}ok $index - [full-name]"
            }
            if {${-code} || $::verbose} {
              set pad "${::pad}  "
              puts "${pad}---"
              puts "${pad}root: [pwd]"
              puts "${pad}source: [file tail $::argv0]"
              if {[llength $tags]} {puts "${pad}tags: $tags"}
              puts "${pad}exit: ${-code}"
              if {${-result} ne ""} {puts "${pad}return: ${-result}"}
              puts-yaml-block $pad stdout ${-stdout}
              puts-yaml-block $pad stderr ${-stderr}
              puts "${pad}..."
            }
          }
          markdown {
            if {${-code} || $::verbose} {
              puts {}
              puts "---"
              puts "## [full-name]"
            }
            if {${-code}} {puts "***FAILURE*** -- ${-result}"}
            if {${-code} || $::verbose} {
              if {${-result} ne ""} {puts ${-result}}
              puts "- root: [pwd]"
              puts "- source: [file tail $::argv0]"
              if {[llength $tags]} {puts "- tags: $tags"}
              puts "- exit: ${-code}"
              puts-markdown-block stdout ${-stdout}
              puts-markdown-block stderr ${-stderr}
            }
          }
        }
        # Flag the entire harness failure if any of units fail
        if {${-code}} {
          set ::status 1
          lappend failed [self]
        }
      }
      flush stdout
    }

    # Process skipped unit
    method process-skipped {} {
      if {$::progress && !$::quiet} {
        puts -nonewline stderr s
        flush stderr
      }
      set s [list]
      foreach skip $skips {
        switch $skip {
          none {lappend s "none {$::tags::none}"}
          any {lappend s "!any {$::tags::any}"}
          all {lappend s "!all {$::tags::all}"}
        }
      }
      set s [join $s {, }]
      switch $::report {
        tap {
          puts "\n${::pad}ok $index - [full-name] # SKIP filtered out by tag rules -- $s"
          if {$::verbose} {
            set pad "${::pad}  "
            puts "${pad}---"
            puts "${pad}root: [pwd]"
            puts "${pad}source: [file tail $::argv0]"
            if {[llength $tags]} {puts "${pad}tags: $tags"}
            puts "${pad}..."
          }
        }
        markdown {
          if {$::verbose} {
            puts {}
            puts "---"
            puts "## [full-name]"
            puts "***SKIP*** -- $s"
            puts "- root: [pwd]"
            puts "- source: [file tail $::argv0]"
            if {[llength $tags]} {puts "- tags: $tags"}
          }
        }
      }
      flush stdout
    }

    # Unique index to create default unit names
    common count 0

    # Thread pool used to run units' scripts
    common pool [tpool::create -initcmd {
      oo::class create @channel {
        variable stream
        method initialize {handle mode} {return {initialize finalize write}}
        method write {handle buffer} {append stream $buffer; return}
        method reset {} {set stream {}}
        method stream {} {return $stream}
      }
      chan push stdout [@channel create @stdout]
      chan push stderr [@channel create @stderr]
    }]

    # Current list of submitted and unprocessed jobs
    common pending [list]

    # List of submitted units
    public common submitted [list]

    # List of skipped units due to filering rules
    public common skipped [list]

    # List of failed tests
    public common failed [list]

    # Wait for and process the results of all units' jobs
    proc process {} {
      # Process skipped units
      foreach unit $skipped {$unit process-skipped}
      # Process competed units
      while {[llength $pending] > 0} {
        foreach job [tpool::wait $pool $pending pending] {
          set outcome [tpool::get $pool $job]
          [dict get $outcome -this] process-completed $outcome
        }
      }
    }

    # Perform cleanup operations
    proc shutdown {} {
      tpool::release $pool
    }

    #
    proc define {args} {
      set usage {unit ?-option value ...? ?--? code}
      set options [list]
      set params [list]
      while {[llength $args]} {
        switch -glob -- [lindex $args 0] {
          -- {break}
          -* {lappend options {*}[lshift args 2]}
          default {lappend params [lshift args]}
        }
      }
      lappend params {*}$args
      if {![llength $params]} {error $usage}
      set unit [unit #auto {*}$params]
      $unit configure {*}$options
      $unit submit
      return
    }
  }

  # Convert raw tag list to a sorted set
  itcl::configbody unit::tags {set tags [set::create {*}$tags]}

  # Exit status to be returned to OS
  set status 0

  # Output progress to stderr
  set progress 1

  # Suppress all non-report output
  set quiet 0

  # Increaed verbosity
  set verbose 0

  # Report output mode {tap markdown}
  set report {}

  set args $argv

  # report usage to stdout and exit
  set usage 0

  namespace eval tags {
    set any [list]
    set all [list]
    set none [list]
    set separators {:;, }
  }

  # List of the unit's quids to be run
  set quids [list]

  namespace eval quids {
    set submitted [list]
    set skipped [list]
    set failed [list]
  }

  # Parse supplied command line arguments
  # Unknown arguments are silently ignored
  while {[llength $args]} {
    switch -regexp -- [lindex $args 0] {
      {^@[0-9a-f]{4}$} {lappend quids [lshift args]}
      {^--$} {break}
      {^--help$} {set usage 1; break}
      {^--verbose$} {set verbose 1; lshift args}
      {^--tap$} {set report tap; lshift args}
      {^--markdown$} {set report markdown; lshift args}
      {^--progress$} {set progress 1; lshift args}
      {^--quiet$} {set quiet 1; lshift args}
      {^--tags-separators$} {lassign [lshift args 2] _ tags::separators}
      {^(-i|--tags-any)$} {lappend tags::any {*}[split [lindex $args 1] $tags::separators]; lshift args 2}
      {^(-s|--tags-all)$} {lappend tags::all {*}[split [lindex $args 1] $tags::separators]; lshift args 2}
      {^(-x|--tags-none)$} {lappend tags::none {*}[split [lindex $args 1] $tags::separators]; lshift args 2}
      {^-[^-].*$} {
        foreach x [split [lshift args] {}] {
          switch $x {
            h {set usage 1; break}
            q {set quiet 1}
            v {set verbose 1}
          }
        }
      }
      default {lshift args}
    }
  }

  # Convert all tag lists to sets
  namespace eval tags {
    set any [set::create {*}$any]
    set all [set::create {*}$all]
    set none [set::create {*}$none]
  }

  if {$usage} {
    puts "T3 unit testing framework command line options"
    exit
  }

  # The interpreter sandbox which executes the user-supplied code
  interp create playground

  if {[catch {set @nesting}]} {
    # Executed by master toplevel interpreter only which has no @nesting variable set
    switch $report {
      tap {puts "TAP version 14"}
    }
    set @nesting 0
  }

  switch $report {
    tap {if {!${@nesting}} {puts "\n# $argv0"}}
  }
  
  # Space-padding string
  # Each nesting level adds up 4 spaces
  set pad [format %[expr {${@nesting}*4}]s {}]

  try {

    # Flag the playground interpreter to skip configuration and proceed to the user's code
    playground eval set @boxed 1

    playground alias unit unit::define

    try {
      # Re-evaluate current source on the playground
      if {[catch {interp eval playground [subst -nocommands {source [set argv0 {$argv0}]}]} result options]} {
        switch $report {
          tap {puts "\n${pad}1..0 # $result"}
        }
        # Failure during interpreting user-supplied code
        set status 1
      } else {
        unit::process
        switch $report {
          tap {
            set tc [expr {[llength $unit::submitted] + [llength $unit::skipped]}]
            puts "\n${pad}1..$tc"
          }
        }
      }
    } finally {
      # Collect statistics
      foreach unit $unit::submitted {lappend quids::submitted [$unit quid]}
      foreach unit $unit::skipped {lappend quids::skipped [$unit quid]}
      foreach unit $unit::failed {lappend quids::failed [$unit quid]}
      unit::shutdown
    }

    # Compute nesting level for subprojects
    set nesting [expr {${@nesting}+1}]

    # Recursive handling the subprojects sharing this source' name
    foreach source [glob -nocomplain -type f -tails -directory [pwd] [file join * [file tail $argv0]]] {
      set wd [pwd]
      switch $report {
        tap {puts "\n${pad}# Subtest: [file join [file dirname $argv0] $source]"}
      }
      interp create descent
      set code [subst -nocommands {
        set @nesting $nesting
        set argv [list $argv]
        source [set argv0 {$source}]
      }]
      # FIXME proper exception handling
      catch {interp eval descent $code}
      if {[interp eval descent {set status}]} {set status 1}
      lappend quids::submitted {*}[interp eval descent {set quids::submitted}]
      lappend quids::skipped {*}[interp eval descent {set quids::skipped}]
      lappend quids::failed {*}[interp eval descent {set quids::failed}]
      interp delete descent
      cd $wd
    }

  } finally {
    interp delete playground
  }

  if {${@nesting}} {
    # FIXME
    # Termination part of the subproject
    error "exiting from the subproject"
  } else {
    # Termination part of toplevel project
    set tc [expr {[llength $quids::submitted] + [llength $quids::skipped]}]
    set fc [llength $quids::failed]
    if {$progress && !$quiet} {puts stderr "\n[expr {$tc - $fc}]/$tc"}
    if {$verbose && !$quiet} {
      foreach quid $quids::failed {puts stderr $quid}
    }
    exit $status
  }

} else {

  # This part precedes the user-supplied code evaluated on the playground

  cd [file dirname $argv0]

}