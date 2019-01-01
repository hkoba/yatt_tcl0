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
                               [dict get $declDict $partSpec] \
                               $declDict $otherDecls]
        }
        set script
    }

    method transpile-page {partName declRec fileDecl otherDecls} {
        $self transpile-widget $partName $declRec $fileDecl $otherDecls
    }
    method transpile-widget {partName declRec fileDecl otherDecls} {
        set scriptBody []
        foreach tok [$self parse-body [dict get $declRec source]] {
            lappend scriptBody [$self generate $tok]
        }
        return "; proc render__$partName {[list this CON {*}[dict get $declRec atts]]} {[join $scriptBody {; }]}"
    }
    method transpile-action {partName declRec fileDecl otherDecls} {}

    method generate tok {
        $self generate-[lindex $tok 0] $tok
    }
    method generate-text tok {
        set text [lindex $tok 1]
        return "\$CON write [list $text]"
    }
    method generate-pi tok {
        lindex $tok 1
    }
    method generate-entity tok {
        # XXX: entpath is not yet supported
        set varName [regsub ^: [lindex $tok 1] {}]
        string map [list @VAR@ $varName] {$CON write [escape $@VAR@]}
    }
    method generate-call tok {
        # XXX: body is not yet used
        # XXX: $this is passed
        lassign $tok _ tag_and_args body
        set args [lassign $tag_and_args tag]
        regsub {^\w+:} $tag {} tag
        string map [list @TAG@ $tag @ARGS@ $args] {@TAG@ @ARGS@}
    }

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
    method parse-body source {
        set tokList [$self tokenize-body $source]
        $self organize-body tokList
    }
    method organize-body {tokListVar {outerTag ""}} {
        upvar 1 $tokListVar tokList
        set result []
        while {[llength $tokList]} {
            set tokList [lassign $tokList tok]
            switch [lindex $tok 0] {
                text - entity - pi {
                    lappend result $tok
                }
                tag {
                    set tagSpec [lindex $tok 1]
                    if {[regsub ^/ [lindex $tagSpec 0] {} tag]} {
                        if {$tag ne $outerTag} {
                            error "Tag $outerTag is closed by $tag"
                        }
                        break
                    } elseif {[regsub {/$} $tagSpec {} tagSpec]} {
                        # <yatt:foo />
                        lappend result [list call $tagSpec]
                    } else {
                        # <yatt:foo> ... </yatt:foo>
                        lappend result [list call $tagSpec [$self organize-body tokList [lindex $tagSpec 0]]]
                    }
                }
            }
        }
        set result
    }
    method tokenize-body html {
        set result []
        foreach {text tok} [textutil::splitx $html [$self re body]] {
            if {$text ne ""} {
                lappend result [list text $text]
            }
            if {$tok eq ""} {
                ; # nop
            } elseif {[regsub {^&\w+} $tok {} tok]} {
                lappend result [list entity [regsub {;$} $tok {}]]
            } elseif {[regsub {^<\?\w+} $tok {} tok]} {
                lappend result [list pi [regsub {\?>$} $tok {}]]
            } elseif {[regsub {^<} $tok {} tok]} {
                lappend result [list tag [regsub {>$} $tok {}]]
            } else {
                error "Unknown token $tok"
            }
        }
        set result
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

