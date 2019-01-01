#!/usr/bin/env tclsh
# -*- coding: utf-8; mode: tcl -*-

package require snit
package require textutil

snit::type yatt_tcl {
    option -doc-root
    
    variable myMtime -array []
    
    method render {widgetName argDict} {
        set widgetPath [split $widgetName :]
        
        if {[$self is-compiled $widgetPath]} {
            
        }

    }
    
    option -namespace-list [list yatt tcl]
    method {re decls} {} {
        set reNS [join $options(-namespace-list) |]
        string map [list @NS@ $reNS] {(?x)
            (<!(?:@NS@):[^>]+>\n
            |
             <!--\#(?:@NS@).*?-->)
        }
    }
    method parse-decl html {
        set result [dict create]
        set curPartName [list page ""]
        foreach {text declOrCmmt} [textutil::splitx $html [$self re decls]] {
            if {$text ne ""} {
                dict update result $curPartName curPart {
                    dict lappend curPart source [list text $text]
                }
            }
            if {[regexp ^<!-- $declOrCmmt]} {
                # just ignore
            } elseif {[regexp {^<!\w+((?::\w+}+) ([^>]+)} $declOrCmmt \
                           --> declType attList]} {
                switch $declType {
                    :args {
                        set curPartName [list page ""]
                    }
                    :page - :action {
                        set attArgs [lassign $attList partName]
                        set curPartName [list [string range $declType 1 end] $partType]
                    }
                    default {
                        error "Unknown decltype $declType"
                    }
                }
                dict update result $curPartName curPart {
                    dict lappend curPart decl [list $declType {*}$attList]
                }
            } else {
                error "Really?"
            }
        }
        set result
    }
    
    option -encoding utf-8
    
    method read_file {fn args} {
        set fh [open $fn]
        scope_guard fh [list close $fh]
        fconfigure $fh -encoding $options(-encoding) {*}$args
        read $fh
    }
    
    proc scope_guard {varName command} {
	upvar 1 $varName var
	uplevel 1 [list trace add variable $varName unset \
		       [list apply [list args $command]]]
    }

}

