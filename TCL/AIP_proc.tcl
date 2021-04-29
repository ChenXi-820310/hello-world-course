proc get_ac_report { file } {
    set INFO [open $file r]
    while { [gets $INFO line] >= 0 } {
        if {[regexp {^#} $line]} { puts "##ForACRep: $line" ; continue }
        if {![regexp {set_input_delay|set_output_delay} $line]} { puts "##ForACRep: $line" ; continue }
        puts "#ConSetSDC $line"

        regsub -all {\t+} $line " " line
        regsub -all {\s+} $line " " line
        regexp {(\[get_ports.*\])\s+} $line dummy port
        set edge "" ; set delay "max"
        
        set CmdLine [split $line]
        for { set i 0 } {$i <= [llength $CmdLine]} { incr i} {
            set tmp [lindex $CmdLine $i]
            if { [regexp {\-clock} $tmp] } { 
                incr i ; set clock [lindex $CmdLine $i] ; set edge ""
                if { [regexp {clock_fall} $tmp] } { set edge "fall" }
            } elseif { [regexp {\-min} $tmp] }   { 
                set delay "min" 
            }
        }

        if { [regexp {set_input_delay} $line] } { 
            if { $edge == "" } { 
                puts "report_timing -delay ${delay} -from ${clock} -thr $port"
            } else {
                puts "report_timing -delay ${delay} -fall_from ${clock} -thr $port"
            }

        } elseif { [regexp {set_output_delay} $line] } {
            if { $edge == "" } { 
                puts "report_timing -delay ${delay} -to ${clock} -thr $port"
            } else {
                puts "report_timing -delay ${delay} -fall_to ${clock} -thr $port"
            }
        }

    }

    close $INFO
}


proc coll_to_list { coll } {
	foreach_in_collection element [sort_collection $coll "full_name"] {
		puts "[get_object_name $element]"
	}
}

proc get_clock_property { {clock_list - }} {
    array set ClockArray {};
    foreach_in_collection element [get_clocks *] {
        set is_generated [get_attr [get_clocks $element] is_generated -quiet] 
        if {$is_generated == ""} { set is_generated "-" }

        set master_clock [get_attr [get_clocks $element] master_clock -quiet]
        if { [sizeof_collection $master_clock] == 0 } { 
            set master_clock "-" 
        } else {
            set master_clock [get_object_name $master_clock]
        }

        set sources [get_attr [get_clocks $element] sources -quiet]
        if { [sizeof_collection $sources] == 0 } { 
            set sources "-" 
        } else {
            set sources [get_object_name $sources]
        }
        
        set period [get_attr [get_clocks $element] period] 
        set name [get_attr [get_clocks $element] full_name] 
        set freq [expr 1000/${period}]
        if { ![info exists ClockArray($name)] } { 
            set ClockArray($name) [format "%s\t%3f\t%3f\t%s\t%s\t%s" $name $freq $period $is_generated $master_clock $sources]
        } else {
           puts "Error : clock : $name"
        }
    }

    puts "[format "%s\t%s\t%s\t%s\t%s\t%s" "clock_name" "frequence"  "period" "is_genenrated" "master_clock" "sources"]"
    if { [string compare $clock_list "-"] }  {
        set INFO [open $clock_list r]
        while { [gets $INFO line] >= 0 } {
            set name [lindex [split $line] 0 ]
            if { [info exists ClockArray($name)]} { 
                puts "$ClockArray($name)" 
            } else {
                puts [format "%s\t%s\t%s\t%s\t%s\t%s" $name "-" "-" "-" "-" "-"]
            }
        }
        close $INFO
    } else {
        foreach name [array names ClockArray] {
            puts "$ClockArray($name)"
        }
    }
}






proc get_pin_func { file } {
    set INFO [open $file r]

    array set func_array {}
    while {[gets $INFO line] >= 0} {
        if { [regexp {^#} $line] } { continue }
        set pin_name [lindex [split $line "\,"] 3]
        set func_name [lindex [split $line "\,"] 5]

        ##puts "$pin_name : $func_name"
        if { [regexp {\-} $func_name] }  { continue }
        if { ![regexp {\w+} $func_name] }  { continue }

        regexp {(.*)_(.*)} $pin_name dummy var1 var2
        set var1 [string tolower $var1]
        set var2 [string tolower $var2]
        if {[string length $var2] == 1} { set var2 0${var2} }
        set pin_name q${var1}_${var2}

        if { ![info exists func_array($func_name)] } {
            set func_array($func_name) $pin_name
        } else {
            lappend func_array($func_name) $pin_name
        }
    }

    foreach element [lsort -unique [array names func_array]] {
        puts "$element : \[list [lsort $func_array($element)]\]"
    }

    close $INFO
    array unset func_array

}

proc get_hier_name { instance } {
    set mod ""
    if { [llength [split $instance {\/}]] == 1 } { 
        set mod $instance
    } else {
        set mod [lindex [split $instance {\/}] 0]
    }

    return $mod
}


proc get_inst_clock { inst_clock_pin } { #set inst_clock_pin "hier_inst/clk"

    puts "###### $inst_clock_pin"

    set fi_col [get_pins -quiet [all_fanin -to [get_pins $inst_clock_pin ] -flat -startpoints_only ] ]
    
    set clk_list ""
    set clk_source_list ""
    foreach_in_collection clock_tmp [get_clocks * ] {
      set clk_obj    [get_object_name $clock_tmp ]
    
      if { [get_attribute $clock_tmp sources -quiet] == "" } { continue }
      set clk_source_obj [get_object_name [get_attribute $clock_tmp sources] ]
      set clk_source_arr($clk_obj) ""
      set clk_arr($clk_source_obj) ""
    }
    
    foreach_in_collection clock_tmp [get_clocks * ] {
      set clk_obj        [get_object_name $clock_tmp ]
      if { [get_attribute $clock_tmp sources -quiet] == "" } { continue }
      set clk_source_obj [get_object_name [get_attribute $clock_tmp sources] ]
      set clk_source_arr($clk_obj) ""
      lappend clk_list $clk_obj
      lappend clk_source_arr($clk_obj) $clk_source_obj
      lappend clk_arr($clk_source_obj) $clk_obj
      lappend clk_source_list $clk_source_arr($clk_obj) }
    
    set inst_clock_list ""
    foreach_in_collection fi_tmp [get_pins $fi_col] {
      set fi_obj [get_object_name $fi_tmp]
      if { [lsearch $clk_source_list $fi_obj ] > -1 } {
        lappend inst_clock_list $fi_obj
      }
    }
    
    
    set inst_clock_name($inst_clock_pin) ""
    foreach list_tmp $inst_clock_list {
      lappend inst_clock_name($inst_clock_pin) $clk_arr($list_tmp) }
    

    foreach element $inst_clock_name($inst_clock_pin) {
        set sour [get_object_name [get_attri [get_clocks $element] sources -quiet]]
        puts "$element : $sour"
    }
    #return $inst_clock_name($inst_clock_pin) 

    ##puts "$inst_clock_name($inst_clock_pin)"

    puts ""
}

proc get_driver { pin } {

    if { [regexp {out} [get_attr [get_pins $pin] direction]] } { 
        if { [get_attr [get_pins $pin] is_hierarchical] == false } {
            puts $pin
            return 
        }
    }

    foreach_in_collection element [sort_collection [get_pins -of [get_nets -seg -of [get_pins $pin]]] "full_name"] {
        set name [get_object_name $element]
        #puts $name
    }

    set driver [get_pins -leaf -of [get_nets -seg -of [get_pins $pin] ] -filter "direction =~ *out"]
    puts "\n[get_object_name $driver]"
}



# report_case_propagation.tcl - trace case analysis back to source(s)
# chrispy@synopsys.com
#
# v1.0  05/25/2007  chrispy
#  initial release
# v1.01 05/31/2007  chrispy
#  fix bug with parallel arcs between two pins

proc report_case_propagation {args} {
 parse_proc_arguments -args $args results

 set objects {}
 append_to_collection objects [get_pins -quiet $results(pins_ports)]
 append_to_collection objects [get_ports -quiet $results(pins_ports)]

 # make a list of startpoint contexts to queue up
 # (I use a flat context list instead of succumbing to the lure of recursion...)
 set queued_contexts {}
 foreach_in_collection object $objects {
  lappend queued_contexts [list $object 0]
 }

 array unset visited_from_pins
 array unset visited_pairs
 while {$queued_contexts ne {}} {

  # pop next to_pin off list
  set context [lindex $queued_contexts 0]
  foreach {to_pin indent_level} $context {}
  set queued_contexts [lrange $queued_contexts 1 end]
  set to_pin_name [get_object_name $to_pin]

  # get list of valid from_pins with case values
  set valid_from_pins {}
  if {[set user_case_value [get_attribute -quiet $to_pin user_case_value]] eq {}} {
   foreach_in_collection arc [get_timing_arcs -quiet -to $to_pin] {
    if {[set from_pin [filter_collection [get_attribute $arc from_pin] {defined(case_value)}]] ne {}} {
     append_to_collection -unique valid_from_pins $from_pin
    }
   }
  }

  # from the valid from_pins, get list of the from_pins for all the
  # unvisited backwards *arcs* ending at this pin
  # (the visited_pairs() array contains all the backwards arcs we've been down before)
  set unvisited_from_pins {}
  foreach_in_collection from_pin $valid_from_pins {
   set from_pin_name [get_object_name $from_pin]
   if {![info exists visited_pairs([list $from_pin_name $to_pin_name])]} {
    append_to_collection unvisited_from_pins $from_pin
   }
  }

  # print information about this to_pin
  switch [get_attribute $to_pin object_class] {
   pin {
    if {[set ref_info [get_object_name [get_lib_cells -quiet -of [get_cells -quiet -of $to_pin]]]] eq {}} {
     set ref_info [get_attribute [get_cells -quiet -of $to_pin] ref_name]
    }
   }
   port {set ref_info "[get_attribute $to_pin port_direction] port"}
  }
  if {[sizeof_collection $valid_from_pins] >= 1 && [sizeof_collection $unvisited_from_pins]==0} {
   # in this branch, we arrived at this to_pin but all backwards arcs have previously been visited
   set branch_info " (previously covered path)"
  } elseif {[sizeof_collection $valid_from_pins] > 1} {
   # multiple backwards arcs exist, so there are multiple branches to trace back
   set branch_info " (branch [expr {1+[sizeof_collection $valid_from_pins]-[sizeof_collection $unvisited_from_pins]}] of [sizeof_collection $valid_from_pins] follows)"
  } else {
   # just a single unambiguous path to trace back
   set branch_info {}
  }
  if {$user_case_value ne {}} {
   set case_info "user-defined case=$user_case_value"
  } else {
   set case_info "case=[get_attribute -quiet $to_pin case_value]"
  }
  echo "[string repeat { } $indent_level][get_object_name $to_pin] ($ref_info) $case_info${branch_info}"

  # restart loop with next to_pin context if there's nothing to trace back
  # (nothing to queue up)
  if {$unvisited_from_pins eq {}} {
   continue
  }

  # first, grab the first from_pin on the list
  set from_pin [index_collection $unvisited_from_pins 0]
  set from_pin_name [get_object_name $from_pin]
  set unvisited_from_pins [remove_from_collection $unvisited_from_pins $from_pin]

  # if there are any additional remaining from_pins, queue *this* to_pin up again
  # so that we can trace back those other branches too
  if {$unvisited_from_pins ne {}} {
   set queued_contexts [linsert $queued_contexts 0 [list $to_pin $indent_level]]
  }

  # now, push our first unvisited from_pin onto the list of to_pins to visit next
  # (this moves us backwards down this path, and the fact that we push it last
  # causes the depth-first backtrace)
  if {[sizeof_collection $valid_from_pins] > 1} {
   incr indent_level
  }
  set queued_contexts [linsert $queued_contexts 0 [list $from_pin $indent_level]]

  # mark this arc as visited (so we never propagate past it in the future)
  set visited_pairs([list $from_pin_name $to_pin_name]) 1
 }

 echo ""
}

define_proc_attributes report_case_propagation \
 -info "trace case analysis back to source(s)" \
 -define_args \
 {
  {pins_ports {pin(s)/port(s) where case value is present} pins_ports string required}
 }


proc get_lib_cell_list {lib_name} {

    foreach_in_collection lib_cell [sort_collection [get_lib_cells ${lib_name}/*] "full_name"] {
        set base_name [get_attribute -class lib_cell ${lib_cell} base_name]

        set dont_use ""
        if {[regexp {_X0P[0-9]} $base_name] } { set dont_use true }

        set footpin [lindex [split $base_name "_"] 0]
        set strength [lindex [split $base_name "_"] 1]
        puts "$footpin,$strength,$base_name,$dont_use"

    }
}

proc expand_net_segment_dont_touch { file } {
    ##upvar collection_result_display_limit collection_result_display_limit
    ##set tmp_limit $collection_result_display_limit
    ##set collection_result_display_limit 10000

    set INFO [open $file r]
    while { [gets $INFO line] >= 0 } {
        if {![regexp {\-seg} $line] || [regexp {^#} $line] } { puts $line ; continue}
        
        if {![regexp {set_dont_touch} $line]} { puts $line ; continue}
        puts "##Exapnd : $line"

        regsub -all {set_dont_touch} $line "sizeof_collection" line_new
        redirect -variable tmp_Number { eval " $line_new " }
        foreach element $tmp_Number { puts "###Net of the Segment Number : $element" ; set sum $element }

        if { [regexp {^\s+set_dont_touch \[} $line] } {
            regsub -all {^\s+set_dont_touch \[} $line "" line
        } else {
            regsub -all {^set_dont_touch \[} $line "" line
        }
        regsub -all {\]$} $line "" line
        redirect -variable tmp_var { eval " $line " }
        regsub -all {\,} $tmp_var " " tmp_var
        regsub -all {\"} $tmp_var " " tmp_var
    
        set num 0 
        foreach element [lsort $tmp_var] { 
            foreach tmp [lsort [split $element " "]] {
                if {[regexp {\w+} $tmp] || [regexp {\d+} $tmp]} { puts "set_dont_touch \[get_nets $tmp\]" ; incr num }
            }
        }
    
        if {[expr $sum - $num] != 0 } { puts "Error ###############"}
        puts ""
    }
    
    close $INFO

}


proc get_pad { } {

    foreach_in_collection pad_pin [sort_collection [get_pins pad/*pad/*] "full_name"] {
        set full_name [get_attr $pad_pin full_name]
        echo "$full_name"
    }


}


proc get_path_sumary { thrpoint {slack_than 1000} } {

    set timing_path_coll1 [get_timing_paths -rise_thr $thrpoint -start_end_pair -slack_lesser_than $slack_than]
    set timing_path_coll2 [get_timing_paths -fall_thr $thrpoint -start_end_pair -slack_lesser_than $slack_than]
    
    set timing_path_coll [add_to_collection $timing_path_coll1 $timing_path_coll2]
    
    puts "start_clk,end_clk,start_point,end_point,slack"

    foreach_in_collection timing_path $timing_path_coll {

        set start_clk [get_attr [get_attr $timing_path startpoint_clock] full_name]
        set start_point [get_attr [get_attr $timing_path startpoint] full_name]

        set end_clk [get_attr [get_attr $timing_path endpoint_clock] full_name]
        set end_point [get_attr [get_attr $timing_path endpoint] full_name]


        foreach_in_collection point [get_attr $timing_path points] {
            set pin_name [get_attr [get_attr $point object] full_name]
            if { [string compare $pin_name $thrpoint] != 0 } { continue }
            set rise_fall [get_attr $point rise_fall -quiet]

            if {$rise_fall == "" } { set rise_fall "-" } 
            break
        }

        set slack [get_attr $timing_path slack]

        puts "$start_clk,$end_clk,$start_point,$end_point,$slack,$rise_fall"
    }
}



proc get_path_slack_info { file } {
    set INFO [open $file r]

    array set func_array {} ; array set wns_array {}
    array set startpoint_array {} ; array set wns_startpoint_array {} 
    array set endpoint_array {} ; array set wns_endpoint_array {}
    
    while {[gets $INFO line] >= 0} {
        if { [regexp {^#} $line] } { continue }
        set startpoint [lindex [split $line "\,"] 1]
        set endpoint   [lindex [split $line "\,"] 2]
        set group [lindex [split $line "\,"] 3]
        set slack [lindex [split $line "\,"] 5]
        set start_clock [lindex [split $line "\,"] 6]
        set end_clock [lindex [split $line "\,"] 7]


        set func_name "${start_clock}:${end_clock}"
        
        
        if { ![info exists func_array($func_name)] } {
            set func_array($func_name) $slack
        } else {
            set func_array($func_name) [expr $slack + $func_array($func_name)]
        }

        if { ![info exists wns_array($func_name)] } {
            set wns_array($func_name) $slack
        } else {
            if {$wns_array($func_name) > $slack } {
                set wns_array($func_name) $slack
            }
        }

        if { ![info exists startpoint_array($startpoint)] } {
            set startpoint_array($startpoint) $slack
        } else {
            set startpoint_array($startpoint) [expr $slack + $startpoint_array($startpoint)]
        }

        if { ![info exists wns_startpoint_array($startpoint)] } {
            set wns_startpoint_array($startpoint) $slack
        } else {
            if {$wns_startpoint_array($startpoint) > $slack } {
                set wns_startpoint_array($startpoint) $slack
            }
        }


        if { ![info exists endpoint_array($endpoint)] } {
            set endpoint_array($endpoint) $slack
        } else {
            set endpoint_array($endpoint) [expr $slack + $endpoint_array($endpoint)]
        }

        if { ![info exists wns_endpoint_array($endpoint)] } {
            set wns_endpoint_array($endpoint) $slack
        } else {
            if {$wns_endpoint_array($endpoint) > $slack } {
                set wns_endpoint_array($endpoint) $slack
            }
        }

    }

    echo "start_clock:endclcok : TNS : WNS";
    foreach element [lsort -unique [array names func_array]] {
        puts "$element : $func_array($element) : $wns_array($element)" 
    }


    echo "point : TNS : WNS : Point"
    foreach element [lsort -unique [array names startpoint_array]] {
        puts "$element : $startpoint_array($element) :  $wns_startpoint_array($element) : StartPoint" 
    }
    
    foreach element [lsort -unique [array names endpoint_array]] {
        puts "$element : $endpoint_array($element) : $wns_endpoint_array($element) : EndPoint" 
    }


    close $INFO
    array unset func_array ; array unset wns_array
    array unset startpoint_array ; array unset endpoint_array
    array unset wns_startpoint_array ; array unset  wns_endpoint_array

}
