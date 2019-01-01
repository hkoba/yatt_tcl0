#!/usr/bin/env tclsh
# -*- coding: utf-8; mode: tcl -*-

package require snit
package require textutil

snit::type yatt_tcl {
    option -doc-root
    option -namespace-list [list yatt tcl]
    option -template-ext .html
    
    variable myCompileCache -array []
    
    method render {widgetName argDict} {
        set widgetPath [split $widgetName :]
        
        if {[$self is-compiled $widgetPath]} {
            
        }

    }
    
    method generate-script {widgetName} {
        set fn $options(-doc-root)/$widgetName$options(-template-ext)
        set declDict [$self parse-decllist [$self read_file $fn]]
        $self transpile $declDict
    }

    method transpile {declDict {otherDecls ""}} {
        set script []
        foreach partSpec [dict keys $declDict] {
            lassign $partSpec kind partName
            append script [$self transpile-$kind $partName \
                               [dict get $declDict $partSpec] $otherDecls]
        }
        set script
    }

    method transpile-page {partName declRec otherDecls} {
        $self transpile-widget $partName $declRec $otherDecls
    }
    method transpile-widget {partName declRec otherDecls} {
        set scriptBody []
        foreach {text tok} [$self parse-body [dict get $declRec source]] {
            if {$text ne ""} {
                lappend scriptBody [list CON write $text]
            }
            if {[regexp {^&\w+:(\w+);} $tok -> varName]} {
                lappend scriptBody [string map [list @VAR@ $varName] {CON write [escape $@VAR@]}]
            } elseif {[regexp {^<\?\w+\s(.*?)\?>$} $tok -> pi]} {
                lappend scriptBody $pi
            } else {
                lappend scriptBody "(($tok))"
            }
        }
        return ";proc render__$partName {[list CON {*}[dict get $declRec atts]]} {[join $scriptBody \;]}"
    }
    method transpile-action {partName declRec otherDecls} {}

    method {re body} {} {
        set reNS [join $options(-namespace-list) |]
        string map [list @NS@ (?:$reNS)] {(?x)
            ((?: & @NS@ :
              [\w\-\.:\(\$\)\[\]]+
              ;)
            |
              <(?: \? @NS@  [^\?\>]* \?
                | /?  @NS@ (?:(?::\w+)+) [^>]*
                )>
              )
        }
    }
    method parse-body html {
        textutil::splitx $html [$self re body]
    }

    method {re decls} {} {
        set reNS [join $options(-namespace-list) |]
        string map [list @NS@ (?:$reNS)] {(?x)
            (<! @NS@ :[^>]+>\n
            |
             <!--\# @NS@ .*?-->)
        }
    }
    method parse-decllist html {
        set result [dict create]
        set curPartName [list page ""]
        foreach {text declOrCmmt} [textutil::splitx $html [$self re decls]] {
            if {$text ne ""} {
                dict update result $curPartName curPart {
                    dict append curPart source $text
                }
            }
            if {[regexp ^<!-- $declOrCmmt]} {
                # just ignore
            } elseif {[regexp {^<!\w+((?::\w+)+) ([^>]+)} $declOrCmmt \
                           --> declType attList]} {
                # XXX: <!yatt:args>
                switch $declType {
                    :args {
                        set declKind page
                        set curPartName [list $declKind ""]
                    }
                    :page - :widget - :action {
                        set declKind [string range $declType 1 end]
                        set attArgs [lassign $attList partName]
                        set curPartName [list $declKind $partName]
                    }
                    default {
                        error "Unknown decltype $declType"
                    }
                }
                dict update result $curPartName curPart {
                    dict set curPart kind $declKind
                    dict set curPart atts $attList
                }
            } elseif {$declOrCmmt ne ""} {
                error "Really? $declOrCmmt"
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

