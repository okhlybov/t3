# https://github.com/okhlybov/t3

# ... and now the True Test (tm)
#     Hold fast or expire ...

package provide T3 0.1.0

if {$tcl_interactive} {error {T3 package can not be used from within interactive session}}

package require Tcl 8.6
package require Thread
package require Itcl

if {[catch {set @boxed}]} {

  # The code below is executed by the master interpreter

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

    proc puts-yaml-multiline {pad tag output} {
      set lines [split $output "\n"]
      if {[lindex $lines end] eq ""} {set lines [lrange $lines 0 end-1]}
      switch [llength $lines] {
        0 {}
        1 {puts "${pad}${tag}: [lindex $lines 0]"}
        default {
          puts "${pad}${tag}: |"
          foreach line $lines {puts "${pad}  $line"}
        }
      }
    }

    # Analyze the completed job's outcome
    method analyze {outcome} {
      dict with outcome {
        switch $::output {
          tap {
            if {${-code}} {
              puts "\n${::pad}not ok $index - $name # ${-result}"
              set pad "${::pad}  "
              puts "${pad}---"
              puts "${pad}root: [pwd]"
              puts "${pad}source: [file tail $::argv0]"
              puts "${pad}exit: ${-code}"
              puts "${pad}return: ${-result}"
              puts-yaml-multiline $pad stdout ${-stdout}
              puts-yaml-multiline $pad stderr ${-stderr}
              puts "${pad}..."
            } else {
              puts "\n${::pad}ok $index - $name"
            }
          }
          default {
            switch ${-code} {
              0 {puts -nonewline .}
              default {puts -nonewline F}
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

  # Results output mode: {default tap}
  set output default

  # Tth interpreter sandbox which executes the user-supplied code
  interp create playground

  if {[catch {set @nesting}]} {
    # Executed by master toplevel interpreter only which has no @nesting variable set
    switch $output {
      tap {puts {TAP version 14}}
    }
    set @nesting 0
  }

  switch $output {
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
        switch $output {
          tap {puts "\n${pad}1..0 # $result"}
        }
        # Failure during interpreting user-supplied code
        set status 1
      } else {
        unit::process
        switch $output {
          tap {puts "\n${pad}1..[llength $unit::submitted]"}
        }
      }
    } finally {
      # Collect statistics
      set submitted [llength $unit::submitted]
      set failed [llength $unit::failed]
      unit::shutdown
    }

    # Compute nesting level for subprojects
    set nesting [expr {${@nesting}+1}]

    # Recursive handling the subprojects sharing this source' name
    foreach source [glob -nocomplain -type f -tails -directory [pwd] [file join * [file tail $argv0]]] {
      set wd [pwd]
      switch $output {
        tap {puts "\n${pad}# Subtest: [file join [file dirname $argv0] $source]"}
      }
      interp create descent
      set code [subst -nocommands {
        set @nesting $nesting
        source [set argv0 {$source}]
      }]
      # FIXME proper exception handling
      catch {interp eval descent $code}
      if {[interp eval descent set status]} {set status 1}
      incr submitted [interp eval descent {set submitted}]
      incr failed [interp eval descent {set failed}]
      interp delete descent
      cd $wd
    }

  } finally {
    interp delete playground
  }

  if {${@nesting}} {
    # Termination part of the subproject
    error
  } else {
    # Termination part of toplevel project
    switch $output {
      default {puts "\n[expr {$submitted - $failed}]/$submitted"}
    }
    exit $status
  }

} else {

  # This part precedes the user-supplied code evaluated on the playground

  cd [file dirname $argv0]

}