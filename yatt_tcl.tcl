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

    method transpile {declDict} {
        set script []
        foreach partSpec [dict keys $declDict] {
            lassign $partSpec kind partName
            append script [$self transpile-$kind $partName \
                               [dict get $declDict $partSpec] \
                               $declDict]
        }
        set script
    }

    method transpile-page {partName declRec fileDecl} {
        $self transpile-widget $partName $declRec $fileDecl
    }
    method transpile-widget {partName declRec fileDecl} {
        set scriptBody []
        foreach tok [$self parse-body [dict get $declRec source]] {
            lappend scriptBody [$self generate $tok]
        }
        set argList [list CON]
        set argDict [dict get $declRec atts]
        set hasDefault []
        foreach argName [dict keys $argDict] {
            set argSpec [dict get $argDict $argName]
            if {[set dflag [dict get $argSpec dflag]] ne "!"} {
                lappend hasDefault $argName
                lappend argList [list $argName [dict get $argSpec default]]
            } else {
                if {[llength $hasDefault]} {
                    error "Mandatory argument $argName is declared after [lindex $hasDefault end] which has default value!"
                }
                lappend argList $argName
            }
        }
        return "; proc render__$partName [list $argList] {[join $scriptBody {; }]}"
    }
    method transpile-action {partName declRec fileDecl} {}

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

    method build-arg-decls attList {
        set dict [dict create]
        foreach nvPair $attList {
            lassign $nvPair varName varSpec
            if {$varName eq ""} {
                error "Name-less attvalue is not allowed in argument list: $nvPair of [list $attList]"
            }
            # type
            # | ? / !
            # default
            lassign [$self parse-type-dflag-default $varSpec text] \
                typeName defaultingMode defaultValue
            dict set dict $varName \
                [dict create type $typeName dflag $defaultingMode \
                     default $defaultValue]
        }
        set dict
    }

    method parse-type-dflag-default {varSpec default} {
        if {[regexp -indices {[|/?!]} $varSpec match]} {
            lassign $match matchBegin
            list [string-or \
                      [string range $varSpec 0 [expr {$matchBegin-1}]] \
                      $default] \
                [string range $varSpec {*}$match] \
                [string range $varSpec [expr {$matchBegin+1}] end]
        } else {
            set default
        }
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
                           --> declType rawAttList]} {
                set attList [$self parse-attlist $rawAttList]
                # XXX: <!yatt:args>
                switch $declType {
                    :args {
                        set declKind page
                        set curPartName [list $declKind ""]
                        if {[dict exists $result $curPartName atts]} {
                            error "!yatt:args must appear only once!"
                        }
                    }
                    :page - :widget - :action {
                        set attList [lassign $attList partName]
                        set declKind [string range $declType 1 end]
                        set curPartName [list $declKind $partName]
                    }
                    default {
                        error "Unknown decltype $declType"
                    }
                }
                dict update result $curPartName curPart {
                    dict set curPart kind $declKind
                    dict set curPart atts \
                        [$self build-arg-decls $attList]
                }
            } elseif {$declOrCmmt ne ""} {
                error "Really? $declOrCmmt"
            }
        }
        set result
    }

    option -debug 0
    option -debug-fh stdout
    method debugLevel {debugLevel args} {
        if {$options(-debug) >= $debugLevel} {
            puts $options(-debug-fh) $args
        }
    }

    # yt parse-attlist { x = "foobar" y='bar' z "foo"}
    method parse-attlist string {
        $self debugLevel 1 "orig=($string)"
	set result {}
        set queue []
        set flushProc [list apply {{resultVar queueVar} {
            upvar 1 $resultVar result
            upvar 1 $queueVar queue
            if {![llength $queue]} return
            lassign [lindex $queue 0] type text
            set queue []
            if {$type eq "IDENT"} {
                # Flush as queued match as value-less attName
                lappend result [list $text]
            } else {
                # Flush as queued match as nameless-attValue
                lappend result [list "" $text]
            }
        }} result queue]

	for {
	    set start 0
	} {$start < [string length $string] &&
	   [regexp -expanded -indices -start $start {
	       \A (?: (\s+) | (=) | (\w+) | "([^\"]*)" | '([^\']*)' | ([^\"\'\>/\s=]+))
	   } $string ALL \
                SPC EQUAL IDENT DQUO QUOT BARE]} {
	    set start [lindex $ALL end]
	    incr start
            $self debugLevel 1 "matched=([extract-match $string $ALL]).next=([string range $string $start end]), queue=($queue), result=($result)"
	} {
            if {[is-nonempty-match $SPC]} continue
            
            if {[is-nonempty-match $EQUAL]} {
                if {[llength $queue] != 1} {
                    error "Invalid '=' in pos=[lindex $EQUAL 0], whole attlist=[list $string]"
                } 
                lassign [lindex $queue 0] type text
                if {$type eq "IDENT"} {
                    lappend queue =
                } else {
                    # Flush as queued match as nameless-attValue
                    set queue []
                    lappend result [list "" $text]
                }
            } else {
                set typedMatch [extract-typed-match $string IDENT DQUO QUOT BARE]

                switch [llength $queue] {
                    0 {
                        lappend queue $typedMatch
                    }
                    1 {
                        {*}$flushProc
                        lappend queue $typedMatch
                    }
                    2 {
                        # In this case, queue[0] must be an IDENT
                        lassign [lindex $queue 0] - attName
                        lassign $typedMatch - attValue
                        set queue []
                        lappend result [list $attName $attValue]
                    }
                    default {
                        error "Really? start=$start, queue=$queue, typedMatch=$typedMatch"
                    }
                }
            }
	}
        {*}$flushProc
	set result
    }
    proc is-nonempty-match match {
        expr {[lindex $match 0] >= 0}
    }
    proc extract-match {string match} {
	if {[lindex $match 0] < 0} return
	string range $string {*}$match
    }
    proc extract-typed-match {string typeVar args} {
        foreach typeVar [list $typeVar {*}$args] {
            upvar 1 $typeVar match
            if {[lindex $match 0] < 0} continue
            return [list $typeVar [string range $string {*}$match]]
        }
    }

    proc string-or {str args} {
	foreach str [list $str {*}$args] {
	    if {$str ne ""} {
		return $str
	    }
	}
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

