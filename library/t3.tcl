# https://github.com/okhlybov/t3

# ... and now the True Test (tm)
#     Hold fast or expire ...

package provide T3 0.1.0

if {$tcl_interactive} {error "T3 package can not be used from within the interactive session"}

package require Tcl 8.6
package require Thread
package require Itcl

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
    if { ! [info exists var] } {
        return -level 1 -code error [subst {can't read "$listVar": no such variable}]
    }
    switch -exact -- [llength $args] {
      1 { set var [lassign $var value] }
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

  # Async unit
  itcl::class unit {

    # Unit unique index
    public variable index

    # Unit name
    public variable name

    # Set of assigned tags
    public variable tags [list]

    # Setup code block
    public variable setup {}

    # Cleanup code block
    public variable cleanup {}

    # Main code block
    public variable code

    constructor {code} {
      incr count
      set index $count
      set name @$count
      configure -code $code
    }

    # Construct a (quasi) unique #tag for unit
    method quid {} {
      return #[format %04x [crc16ish "$name[pwd]$::argv0"]]
    }

    # Submit unit's job
    method submit {} {
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

    # Chop triling empty lines from the list
    private proc chop-trail {list} {
      set x [llength $list]; incr x -1
      for {set i [expr {[llength $list]-1}]} {$i >= 0} {incr i -1} {
        if {[string trim [lindex $list $i]] ne ""} {break}
      }
      if {$i} {return [lrange $list 0 $i]} else {return [list]}
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
        1 {puts "- $tag: $lines"}
        default {
          puts "- $tag:"
          puts "    ~~~~"
          foreach line $lines {puts "    $line"}
          puts "    ~~~~"
        }
      }
    }

    # Analyze the completed job's outcome
    method analyze {outcome} {
      # Passed units' details are output in verbose mode only
      dict with outcome {
        if {$::progress && !$::quiet} {
            switch ${-code} {
              0 {puts -nonewline stderr .}
              default {puts -nonewline stderr F}
            }
        }
        switch $::report {
          tap {
            if {${-code}} {
              puts "\n${::pad}not ok $index - $name # ${-result} [quid]"
            } else {
              puts "\n${::pad}ok $index - $name [quid]"
            }
            if {${-code} || $::verbose} {
              set pad "${::pad}  "
              puts "${pad}---"
              puts "${pad}root: [pwd]"
              puts "${pad}source: [file tail $::argv0]"
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
              puts "## $name [quid]"
            }
            if {${-code}} {puts "***FAILURE***"}
            if {${-code} || $::verbose} {
              if {${-result} ne ""} {puts ${-result}}
              puts "- root: [pwd]"
              puts "- source: [file tail $::argv0]"
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

    # List of failed tests
    public common failed [list]

    # Wait for and process the results of all pending units' jobs
    proc process {} {
      while {[llength $pending] > 0} {
        foreach job [tpool::wait $pool $pending pending] {
          set outcome [tpool::get $pool $job]
          [dict get $outcome -this] analyze $outcome
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

  # Parse supplied command line arguments
  # Unknown arguments are silently ignored
  while {[llength $args]} {
    switch -regexp -- [lindex $args 0] {
      --help {set usage 1; break}
      --verbose {set verbose 1; lshift args}
      --tap {set report tap; lshift args}
      --markdown {set report markdown; lshift args}
      --progress {set progress 1; lshift args}
      --quiet {set quiet 1; lshift args}
      -- {break}
      {-[^-].*} {
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

    # Lists of quids
    set submitted [list]
    set failed [list]

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
          tap {puts "\n${pad}1..[llength $unit::submitted]"}
        }
      }
    } finally {
      # Collect statistics
      foreach unit $unit::submitted {lappend submitted [$unit quid]}
      foreach unit $unit::failed {lappend failed [$unit quid]}
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
      lappend submitted {*}[interp eval descent {set submitted}]
      lappend failed {*}[interp eval descent {set failed}]
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
    set sc [llength $submitted]
    set fc [llength $failed]
    if {$progress && !$quiet} {puts stderr "\n[expr {$sc - $fc}]/$sc"}
    if {$verbose && !$quiet} {
      foreach quid $failed {puts stderr $quid}
    }
    exit $status
  }

} else {

  # This part precedes the user-supplied code evaluated on the playground

  cd [file dirname $argv0]

}