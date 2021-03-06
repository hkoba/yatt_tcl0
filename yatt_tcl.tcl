#!/usr/bin/env tclsh
# -*- coding: utf-8; mode: tcl -*-

package require snit
package require textutil

namespace eval yatt_tcl {
    snit::type outbuf {
        variable myBuffer
        method write args {
            append myBuffer {*}$args
        }
        method value {} {
            set myBuffer
        }
    }
    
    proc escape text {
        string map {< &lt; > &gt; & &amp; \" &quot;} $text
    }
}

snit::type yatt_tcl {
    option -yatt-namespace-list [list yatt tcl]
    option -template-ext .ytcl
    option -tcl-namespace {}
    
    variable myCompileCache [dict create]
    
    option -template-dirs []
    variable myTemplateDirList []
    onconfigure -template-dirs list {
        set myTemplateDirList $list
    }

    method render_file {fileName args} {
        if {[llength $args] % 2 != 0} {
            error "Odd number of arguments!"
        }
        if {[file extension $fileName] ne $options(-template-ext)} {
            error "Unsupported file type: [file extension $fileName]"
        }

        if {[lsearch -exact $myTemplateDirList \
                 [set theDir [file dirname [file normalize $fileName]]]] < 0} {
            set myTemplateDirList \
                [linsert $myTemplateDirList 0 $theDir]
        }

        $self render [file rootname [file tail $fileName]] {*}$args
    }

    method render {widgetName args} {
        if {[llength $args] % 2 != 0} {
            error "Odd number of arguments!"
        }
        
        set CON [yatt_tcl::outbuf $self.%AUTO%]
        scope_guard CON [list rename $CON ""]
        $self render_into $CON $widgetName $args
        
        $CON value
    }
    
    method render_into {CON widgetName argDict} {

        # widgetSpec = widgetDecl + fileSpec
        if {[set widgetSpec [$self find-widget-spec $widgetName]] eq ""} {
            error "No such widget: $widgetName"
        }
        
        set callArgs [dict get $widgetSpec atts]
        foreach argName [dict keys $argDict] {
            if {![dict exists $callArgs $argName]} {
                error "Unknown argument! $argName"
            }
            dict set callArgs $argName actual \
                [dict get $argDict $argName]
        }
        

        set widgetProc [dict get $widgetSpec proc]
        $widgetProc $CON {*}[$self gen-actual-callargs $callArgs]
    }
    
    method find-widget-spec {widgetName {ensure_transpiled 1}} {
        set innerName [lassign [split $widgetName :] tmplName]

        $self update-template-cache $tmplName
        
        dict update myCompileCache $tmplName tmplRec {

            if {$ensure_transpiled && ![dict get $tmplRec transpiled]} {

                set script [$self transpile [dict get $tmplRec parts]]
                apply [list {__NS__ __FILE__ __SCRIPT__} {
                    info script $__FILE__
                    namespace eval $__NS__ $__SCRIPT__
                }] [dict get $tmplRec namespace] \
                    [dict get $tmplRec filename] \
                    $script

                dict set tmplRec transpiled 1
            } else {
                error "not evaled"
            }

            dict merge \
                [dict get $tmplRec parts [list widget $innerName]] \
                [dict filter $tmplRec \
                     key namespace filename]
        }

    }

    method update-template-cache tmplName {

        foreach dir $myTemplateDirList {
            # puts [list dir $dir tmplName $tmplName ext $options(-template-ext)]
            set fn $dir/$tmplName$options(-template-ext)

            if {![file exists $fn]} continue

            set mtime [file mtime $fn]
            if {[dict exists $myCompileCache $tmplName]
                &&
                $mtime == [dict get $myCompileCache $tmplName mtime]} {
                
                return 0
            }
        }
        
        if {![file exists $fn]} {
            error "No such template $tmplName"
        }

        set fileNS [join [list $options(-tcl-namespace) $tmplName] ::]
        set declDict [$self parse-decllist [$self read_file $fn] \
                          ${fileNS}::]

        dict set myCompileCache $tmplName \
            [dict create \
                 mtime $mtime \
                 filename $fn \
                 namespace $fileNS \
                 parts $declDict \
                 transpiled 0 \
                ]
        
        # updated
        return 1
    }

    # For debugging aid only.
    method generate-script {tmplName} {
        
        $self update-template-cache $tmplName

        $self transpile [dict get $myCompileCache $tmplName parts]
    }

    method transpile {declDict} {
        set script []
        foreach partSpec [dict keys $declDict] {
            lassign $partSpec kind partName
            set transCtx [$self transcontext create $declDict $partSpec]
            append script [$self transpile-$kind $partName \
                               [dict get $declDict $partSpec] \
                               $transCtx]
        }
        set script
    }

    method {transcontext create} {declDict partSpec} {
        set declRec [dict get $declDict $partSpec]
        lassign $partSpec kind partName
        dict create \
            container $declDict \
            widget [dict merge \
                         $declRec \
                        [dict create kind $kind name $partName]]\
            scope [list [dict get $declRec atts] [dict create]]
        # XXX: filename lineno
    }

    method {transcontext find-var} {transCtx varName} {
        foreach varDict [lreverse [dict get $transCtx scope]] {
            if {[dict exists $varDict $varName]} {
                return [dict get $varDict $varName]
            }
        }
    }

    method {transcontext find-callable-var} {transCtx varName} {
        if {[set varSpec [$self transcontext find-var $transCtx $varName]] eq ""} return
        if {[dict get $varSpec type] eq "code"} {
            return $varSpec
        }
    }

    method {transcontext find-widget} {transCtx widgetName} {
        # XXX: widgetPath (yatt:foo:bar)
        set partSpec [list widget $widgetName]
        if {![regexp : $widgetName]
            && [dict exists $transCtx container $partSpec]} {
            return [dict get $transCtx container $partSpec]
        } else {
            $self find-widget-spec $widgetName
        }
    }

    method transpile-widget {partName declRec transCtx} {
        $self debugLevel 2 "partName=$partName, declRec=($declRec)"        
        set scriptBody [$self generate $transCtx \
                            [$self parse-body [dict get $declRec source]]]
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
    method transpile-action {partName declRec transCtx} {}

    method generate {transCtx tokList} {
        set scriptList ""
        foreach tok $tokList {
            lappend scriptList [$self generate-[lindex $tok 0] $tok $transCtx]
        }
        set scriptList
    }
    method generate-text {tok transCtx} {
        set text [lindex $tok 1]
        return "\$CON write [list $text]"
    }
    method generate-pi {tok transCtx} {
        # ?tcl=は？
        lindex $tok 1
    }
    method generate-entity {tok transCtx} {
        # XXX: entpath is not yet supported
        set varName [regsub ^: [lindex $tok 1] {}]
        if {[set varSpec [$self transcontext find-var $transCtx $varName]] eq ""} {
            error "No such variable: $varName ($transCtx)"
        }
        set expr [$self gen-emittable-[dict get $varSpec type] \
                      \$$varName]
        concat {$CON write } $expr
    }
    method generate-call {tok transCtx} {
        lassign $tok _ tag_and_args bodyToks
        set actualArgs [lassign $tag_and_args widgetName]
        
        if {[set codeVar [$self transcontext find-callable-var $transCtx $widgetName]] ne ""} {
            return "uplevel 1 \$[dict get $codeVar name]"
        } elseif {[set widgetDecl [$self transcontext find-widget $transCtx $widgetName]] ne ""} {
            $self debugLevel 2 "call [list $tok] resolved to [list $widgetDecl]"
            set callArgs [dict get $widgetDecl atts]
            foreach argTok $actualArgs {
                if {[llength $argTok] == 2} {
                    lassign $argTok argName argValue
                } else {
                    set argName [lindex $argTok 0]
                    if {[info exists argValue]} {unset argValue}
                }
                if {![dict exists $callArgs $argName]} {
                    error "Unknown argument '$argName' for widget $widgetName"
                }
                dict set callArgs $argName actual \
                    [if {[info exists argValue]} {
                        $self gen-as-[dict get $callArgs $argName type] \
                            $argValue $transCtx
                    } else {
                        # argument pass-thru
                        if {[$self transcontext find-var $transCtx $argName] eq ""} {
                            error "No such variable: $argName"
                        }
                        value \$$argName
                    }]
            }
            
            if {$bodyToks ne ""} {
                dict set callArgs body actual \
                    [list [join [$self generate $transCtx $bodyToks] {; }]]
            }

            set argList [$self gen-actual-callargs $callArgs]
            return "[dict get $widgetDecl proc] \$CON [join $argList]"
        } else {
            error "No such widget: $widgetName. ctx=($transCtx)"
        }
    }

    method gen-actual-callargs callArgs {
        lmap argSpec [dict values $callArgs] {
            if {[dict exists $argSpec actual]} {
                dict get $argSpec actual
            } elseif {[dict get $argSpec dflag] eq "!"} {
                error "Argument [dict get $argSpec name] is missing!"
            } else {
                dict get $argSpec default
            }
        }
    }

    method gen-as-text {string transCtx} {
        list $string
    }
    method gen-emittable-text string {
        string map [list @VAL@ $string] {[yatt_tcl::escape @VAL@]}
    }

    method {re body} {} {
        set reNS [join $options(-yatt-namespace-list) |]
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
                    if {[regsub ^/ $tagSpec {} tag]} {
                        if {$tag ne $outerTag} {
                            error "Tag $outerTag is closed by $tag"
                        }
                        break
                    } else {
                        # XXX: TODO: element style attribute! such as :yatt:else

                        # For <yatt:foo />
                        set emptyElem [regsub {/$} $tagSpec {} tagSpec]

                        regexp {^(\w+(?::\w+)+)(.*)} $tagSpec -> tagName rawAtts

                        set attList [$self parse-attlist $rawAtts]
                        # Drop yatt: first.
                        regsub {^\w+:} $tagName {} tagPath

                        set callTok [list call [list $tagPath {*}$attList]]
                        
                        if {!$emptyElem} {
                            lappend callTok [$self organize-body tokList $tagName]
                        }
                        
                        lappend result $callTok
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
                     default $defaultValue name $varName]
        }
        if {![dict exists $dict body]} {
            dict set dict body \
                [dict create name body type code dflag "" default ""]
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
        set reNS [join $options(-yatt-namespace-list) |]
        string map [list @NS@ (?:$reNS)] {(?x)
            (<! @NS@ :[^>]+>\n
            |
             <!--\# @NS@ .*?-->)
        }
    }
    method parse-decllist {html {nsPrefix ""}} {
        set result [dict create]
        set curPartName [list widget ""]
        set otherSpec [list public yes]
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
                        set declKind widget
                        set curPartName [list $declKind ""]
                        set procPrefix  render__
                        if {[dict exists $result $curPartName atts]} {
                            error "!yatt:args must appear only once!"
                        }
                        set otherSpec [list public yes]
                    }
                    :page - :widget - :action {
                        set attList [lassign $attList partName]
                        set declKind [string range [expr {$declType eq ":action" ? $declType : ":widget"}] 1 end]
                        set procPrefix [expr {$declKind eq "widget" ? "render__" : "do__"} ]
                        set curPartName [list $declKind $partName]
                        set otherSpec [list public [expr {$declType ne ":widget"}]]
                    }
                    default {
                        error "Unknown decltype $declType"
                    }
                }
                dict update result $curPartName curPart {
                    dict set curPart kind $declKind
                    dict set curPart proc $nsPrefix$procPrefix[lindex $curPartName end]
                    dict set curPart atts \
                        [$self build-arg-decls $attList]
                    foreach {k v} $otherSpec {
                        dict set curPart $k $v
                    }
                }
            } elseif {$declOrCmmt ne ""} {
                error "Really? $declOrCmmt"
            }
        }
        # Ensure declRec of default (nameless) widget has correct structure
        dict update result [list widget ""] curPart {
            if {![dict exists $curPart atts]} {
                dict set curPart atts {}
                dict set curPart proc ${nsPrefix}render__
            }
        }
        puts [list parsed-decllist: $result]
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

    proc value value {set value}

    proc parsePosixOpts {varName {dict {}}} {
        upvar 1 $varName opts

        for {} {[llength $opts]
                && [regexp {^--?([\w\-]+)(?:(=)(.*))?} [lindex $opts 0] \
                        -> name eq value]} {set opts [lrange $opts 1 end]} {
            if {$eq eq ""} {
                set value 1
            }
            dict set dict -$name $value
        }
        set dict
    }

}

if {![info level] && [info script] eq $::argv0} {
    apply {{} {

        yatt_tcl yt {*}[yatt_tcl::parsePosixOpts ::argv]

        puts [yt {*}$::argv]

    }}
}
