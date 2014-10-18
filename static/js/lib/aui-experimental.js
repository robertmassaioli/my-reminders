;
// tipsy, facebook style tooltips for jquery
// version 1.0.0a
// (c) 2008-2010 jason frame [jason@onehackoranother.com]
// released under the MIT license
//
// Modified by Atlassian
// https://github.com/atlassian/tipsy

(function($) {
    
    function maybeCall(thing, ctx) {
        return (typeof thing == 'function') ? (thing.call(ctx)) : thing;
    };
    
    function isElementInDOM(ele) {
      while (ele = ele.parentNode) {
        if (ele == document) return true;
      }
      return false;
    };

    var tipsyIDcounter = 0;
    function tipsyID() {
        var tipsyID = tipsyIDcounter++;
        return "tipsyuid" + tipsyID;
    };
    
    function Tipsy(element, options) {
        this.$element = $(element);
        this.options = options;
        this.enabled = true;
        this.fixTitle();
    };
    
    Tipsy.prototype = {
        show: function() {
            // if element is not in the DOM then don't show the Tipsy
            if (!isElementInDOM(this.$element[0])) {
                return;
            }

            var title = this.getTitle();
            if (title && this.enabled) {
                var $tip = this.tip();
                
                $tip.find('.tipsy-inner')[this.options.html ? 'html' : 'text'](title);
                $tip[0].className = 'tipsy'; // reset classname in case of dynamic gravity
                $tip.remove().css({top: 0, left: 0, visibility: 'hidden', display: 'block'}).prependTo(document.body);


                var that = this;
                function tipOver() {
                    that.hoverTooltip = true;
                }
                function tipOut() {
                    if (that.hoverState == 'in') return;  // If field is still focused.
                    that.hoverTooltip = false;
                    if (that.options.trigger != 'manual') {
                        var eventOut = that.options.trigger == 'hover' ? 'mouseleave.tipsy' : 'blur.tipsy';
                        that.$element.trigger(eventOut);
                    }
                }
                
                if (this.options.hoverable) {
                    $tip.hover(tipOver, tipOut);
                }

                if (this.options.className) {
                    $tip.addClass(maybeCall(this.options.className, this.$element[0]));
                }

                var pos = $.extend({}, this.$element.offset(), {
                    width: this.$element[0].offsetWidth,
                    height: this.$element[0].offsetHeight
                });
                
                var actualWidth = $tip[0].offsetWidth,
                    actualHeight = $tip[0].offsetHeight,
                    gravity = maybeCall(this.options.gravity, this.$element[0]);
                
                var tp;
                switch (gravity.charAt(0)) {
                    case 'n':
                        tp = {top: pos.top + pos.height + this.options.offset, left: pos.left + pos.width / 2 - actualWidth / 2};
                        break;
                    case 's':
                        tp = {top: pos.top - actualHeight - this.options.offset, left: pos.left + pos.width / 2 - actualWidth / 2};
                        break;
                    case 'e':
                        tp = {top: pos.top + pos.height / 2 - actualHeight / 2, left: pos.left - actualWidth - this.options.offset};
                        break;
                    case 'w':
                        tp = {top: pos.top + pos.height / 2 - actualHeight / 2, left: pos.left + pos.width + this.options.offset};
                        break;
                }
                
                if (gravity.length == 2) {
                    if (gravity.charAt(1) == 'w') {
                        tp.left = pos.left + pos.width / 2 - 15;
                    } else {
                        tp.left = pos.left + pos.width / 2 - actualWidth + 15;
                    }
                }
                
                $tip.css(tp).addClass('tipsy-' + gravity);
                $tip.find('.tipsy-arrow')[0].className = 'tipsy-arrow tipsy-arrow-' + gravity.charAt(0);
                
                if (this.options.fade) {
                    $tip.stop().css({opacity: 0, display: 'block', visibility: 'visible'}).animate({opacity: this.options.opacity});
                } else {
                    $tip.css({visibility: 'visible', opacity: this.options.opacity});
                }

                if (this.options.aria) {
                    var $tipID = tipsyID();
                    $tip.attr("id", $tipID);
                    this.$element.attr("aria-describedby", $tipID);
                }
            }
        },
        
        hide: function() {
            if (this.options.fade) {
                this.tip().stop().fadeOut(function() { $(this).remove(); });
            } else {
                this.tip().remove();
            }
            if (this.options.aria) {
                this.$element.removeAttr("aria-describedby");
            }
        },
        
        fixTitle: function() {
            var $e = this.$element;
            if ($e.attr('title') || typeof($e.attr('original-title')) != 'string') {
                $e.attr('original-title', $e.attr('title') || '').removeAttr('title');
            }
        },
        
        getTitle: function() {
            var title, $e = this.$element, o = this.options;
            this.fixTitle();
            var title, o = this.options;
            if (typeof o.title == 'string') {
                title = $e.attr(o.title == 'title' ? 'original-title' : o.title);
            } else if (typeof o.title == 'function') {
                title = o.title.call($e[0]);
            }
            title = ('' + title).replace(/(^\s*|\s*$)/, "");
            return title || o.fallback;
        },
        
        tip: function() {
            if (!this.$tip) {
                this.$tip = $('<div class="tipsy"></div>').html('<div class="tipsy-arrow"></div><div class="tipsy-inner"></div>').attr("role","tooltip");
                this.$tip.data('tipsy-pointee', this.$element[0]);
            }
            return this.$tip;
        },
        
        validate: function() {
            if (!this.$element[0].parentNode) {
                this.hide();
                this.$element = null;
                this.options = null;
            }
        },
        
        enable: function() { this.enabled = true; },
        disable: function() { this.enabled = false; },
        toggleEnabled: function() { this.enabled = !this.enabled; }
    };
    
    $.fn.tipsy = function(options) {
        
        if (options === true) {
            return this.data('tipsy');
        } else if (typeof options == 'string') {
            var tipsy = this.data('tipsy');
            if (tipsy) tipsy[options]();
            return this;
        }
        
        options = $.extend({}, $.fn.tipsy.defaults, options);
        if (options.hoverable) {
            options.delayOut = options.delayOut || 20;
        }
        
        function get(ele) {
            var tipsy = $.data(ele, 'tipsy');
            if (!tipsy) {
                tipsy = new Tipsy(ele, $.fn.tipsy.elementOptions(ele, options));
                $.data(ele, 'tipsy', tipsy);
            }
            return tipsy;
        }
        
        function enter() {
            var tipsy = get(this);
            tipsy.hoverState = 'in';
            if (options.delayIn == 0) {
                tipsy.show();
            } else {
                tipsy.fixTitle();
                setTimeout(function() { if (tipsy.hoverState == 'in') tipsy.show(); }, options.delayIn);
            }
        };
        
        function leave() {
            var tipsy = get(this);
            tipsy.hoverState = 'out';
            if (options.delayOut == 0) {
                tipsy.hide();
            } else {
                setTimeout(function() { if (tipsy.hoverState == 'out' && !tipsy.hoverTooltip) tipsy.hide(); }, options.delayOut);
            }
        };
        
        if (!options.live) this.each(function() { get(this); });
        
        if (options.trigger != 'manual') {
            var eventIn  = options.trigger == 'hover' ? 'mouseenter.tipsy' : 'focus.tipsy',
                eventOut = options.trigger == 'hover' ? 'mouseleave.tipsy' : 'blur.tipsy';
            if (options.live) {
                $(this.context).on(eventIn, this.selector, enter).on(eventOut, this.selector, leave);
            } else {
                this.bind(eventIn, enter).bind(eventOut, leave);
            }
        }
        
        return this;
        
    };
    
    $.fn.tipsy.defaults = {
        aria: false,
        className: null,
        delayIn: 0,
        delayOut: 0,
        fade: false,
        fallback: '',
        gravity: 'n',
        html: false,
        live: false,
        hoverable: false,
        offset: 0,
        opacity: 0.8,
        title: 'title',
        trigger: 'hover'
    };
    
    $.fn.tipsy.revalidate = function() {
      $('.tipsy').each(function() {
        var pointee = $.data(this, 'tipsy-pointee');
        if (!pointee || !isElementInDOM(pointee)) {
          $(this).remove();
        }
      });
    };
    
    // Overwrite this method to provide options on a per-element basis.
    // For example, you could store the gravity in a 'tipsy-gravity' attribute:
    // return $.extend({}, options, {gravity: $(ele).attr('tipsy-gravity') || 'n' });
    // (remember - do not modify 'options' in place!)
    $.fn.tipsy.elementOptions = function(ele, options) {
        return $.metadata ? $.extend({}, options, $(ele).metadata()) : options;
    };
    
    $.fn.tipsy.autoNS = function() {
        return $(this).offset().top > ($(document).scrollTop() + $(window).height() / 2) ? 's' : 'n';
    };
    
    $.fn.tipsy.autoWE = function() {
        return $(this).offset().left > ($(document).scrollLeft() + $(window).width() / 2) ? 'e' : 'w';
    };
    
    /**
     * yields a closure of the supplied parameters, producing a function that takes
     * no arguments and is suitable for use as an autogravity function like so:
     *
     * @param margin (int) - distance from the viewable region edge that an
     *        element should be before setting its tooltip's gravity to be away
     *        from that edge.
     * @param prefer (string, e.g. 'n', 'sw', 'w') - the direction to prefer
     *        if there are no viewable region edges effecting the tooltip's
     *        gravity. It will try to vary from this minimally, for example,
     *        if 'sw' is preferred and an element is near the right viewable 
     *        region edge, but not the top edge, it will set the gravity for
     *        that element's tooltip to be 'se', preserving the southern
     *        component.
     */
     $.fn.tipsy.autoBounds = function(margin, prefer) {
		return function() {
			var dir = {ns: prefer[0], ew: (prefer.length > 1 ? prefer[1] : false)},
			    boundTop = $(document).scrollTop() + margin,
			    boundLeft = $(document).scrollLeft() + margin,
			    $this = $(this);

			if ($this.offset().top < boundTop) dir.ns = 'n';
			if ($this.offset().left < boundLeft) dir.ew = 'w';
			if ($(window).width() + $(document).scrollLeft() - $this.offset().left < margin) dir.ew = 'e';
			if ($(window).height() + $(document).scrollTop() - $this.offset().top < margin) dir.ns = 's';

			return dir.ns + (dir.ew ? dir.ew : '');
		}
	};
    
})(jQuery);

(function ($) {
    var $document = $(document),

        //convenience function because this needs to be run for all the events.
        getExpanderProperties = function(event){
            var properties = {};

            properties.$trigger = $(event.currentTarget);
            properties.$content = $document.find('#' + properties.$trigger.attr('aria-controls'));
            properties.triggerIsParent = properties.$content.parent().filter(properties.$trigger).length !== 0;
            properties.$shortContent = properties.triggerIsParent ? properties.$trigger.find('.aui-expander-short-content') : null;
            properties.height = properties.$content.css('min-height');
            properties.isCollapsible = properties.$trigger.data('collapsible') !== false;
            properties.replaceText = properties.$trigger.attr('data-replace-text'); //can't use .data here because it doesn't update after the first call
            properties.replaceSelector = properties.$trigger.data('replace-selector');

            return properties;
        },
        replaceText = function(properties) {
            if (properties.replaceText) {
                var $replaceElement = properties.replaceSelector ?
                properties.$trigger.find(properties.replaceSelector) :
                    properties.$trigger;

                properties.$trigger.attr('data-replace-text', $replaceElement.text());
                $replaceElement.text(properties.replaceText);
            }
        };
        //events that the expander listens to
        var EXPANDER_EVENTS = {
            'aui-expander-invoke': function(event) {
                var $trigger = $(event.currentTarget);
                var $content = $document.find('#' + $trigger.attr('aria-controls'));
                var isCollapsible = $trigger.data('collapsible') !== false;

                //determine if content should be expanded or collapsed
                if ($content.attr('aria-expanded') === 'true' && isCollapsible) {
                    $trigger.trigger('aui-expander-collapse');
                } else {
                    $trigger.trigger('aui-expander-expand');
                }
            },
            'aui-expander-expand': function(event) {
                var properties = getExpanderProperties(event);

                properties.$content.attr('aria-expanded', 'true');
                properties.$trigger.attr('aria-expanded', 'true');

                if (properties.$content.outerHeight() > 0) {
                    properties.$content.attr('aria-hidden', 'false');
                }

                //handle replace text
                replaceText(properties);

                //if the trigger is the parent also hide the short-content (default)
                if(properties.triggerIsParent){
                    properties.$shortContent.hide();
                }
                properties.$trigger.trigger('aui-expander-expanded');

            },
            'aui-expander-collapse': function(event){

                var properties = getExpanderProperties(event);

                //handle replace text
                replaceText(properties);

                //collapse the expander
                properties.$content.attr('aria-expanded', 'false');
                properties.$trigger.attr('aria-expanded', 'false');

                //if the trigger is the parent also hide the short-content (default)
                if(properties.triggerIsParent){
                    properties.$shortContent.show();
                }

                //handle the height option
                if (properties.$content.outerHeight() === 0) {
                    properties.$content.attr('aria-hidden', 'true');
                }

                properties.$trigger.trigger('aui-expander-collapsed');
            },

            'click.aui-expander': function(event){
                var $target = $(event.currentTarget);
                $target.trigger('aui-expander-invoke', event.currentTarget);
            }
        };

    //delegate events to the triggers on the page
    $document.on(EXPANDER_EVENTS, '.aui-expander-trigger');

})(jQuery);

//API
(function() {
    'use strict';
    function updateProgress($progressBar, $progressBarContainer, progressValue) {
        AJS._internal.animation.recomputeStyle($progressBar);
        $progressBar.css("width", progressValue * 100 + "%");
        $progressBarContainer.attr("data-value", progressValue);
    }



    AJS.progressBars = {
        update: function(element, value){
            var $progressBarContainer = AJS.$(element).first();
            var $progressBar = $progressBarContainer.children(".aui-progress-indicator-value");
            var currentProgress = $progressBar.attr("data-value") || 0;

            var afterTransitionEvent = "aui-progress-indicator-after-update";
            var beforeTransitionEvent = "aui-progress-indicator-before-update";
            var transitionEnd = "transitionend webkitTransitionEnd";

            var isIndeterminate = !$progressBarContainer.attr("data-value");

            //if the progress bar is indeterminate switch it.
            if (isIndeterminate) {
                $progressBar.css("width", 0);
            }

            if (typeof value === "number" && value<= 1 && value >= 0) {
                $progressBarContainer.trigger(beforeTransitionEvent, [currentProgress, value]);

                //detect whether transitions are supported
                var documentBody = document.body || document.documentElement;
                var style = documentBody.style;
                var transition = 'transition';

                //trigger the event after transition end if supported, otherwise just trigger it
                if (typeof style.transition === 'string' || typeof style.WebkitTransition === "string") {
                    $progressBar.one(transitionEnd, function() {
                        $progressBarContainer.trigger(afterTransitionEvent, [currentProgress, value]);
                    });
                    updateProgress($progressBar, $progressBarContainer, value);
                } else {
                    updateProgress($progressBar, $progressBarContainer, value);
                    $progressBarContainer.trigger(afterTransitionEvent, [currentProgress, value]);
                }


            }
            return $progressBarContainer;
        },
        setIndeterminate: function(element) {
            var $progressBarContainer = AJS.$(element).first();
            var $progressBar = $progressBarContainer.children(".aui-progress-indicator-value");

            $progressBarContainer.removeAttr("data-value");
            AJS._internal.animation.recomputeStyle($progressBarContainer);
            $progressBar.css("width", "100%");
        }
    };
}());


/*!
 * TableSorter 2.10.8 - Client-side table sorting with ease!
 * @requires jQuery v1.2.6+
 *
 * Copyright (c) 2007 Christian Bach
 * Examples and docs at: http://tablesorter.com
 * Dual licensed under the MIT and GPL licenses:
 * http://www.opensource.org/licenses/mit-license.php
 * http://www.gnu.org/licenses/gpl.html
 *
 * @type jQuery
 * @name tablesorter
 * @cat Plugins/Tablesorter
 * @author Christian Bach/christian.bach@polyester.se
 * @contributor Rob Garrison/https://github.com/Mottie/tablesorter
 */
/*jshint browser:true, jquery:true, unused:false, expr: true */
/*global console:false, alert:false */
!(function($) {
    "use strict";
    $.extend({
        /*jshint supernew:true */
        tablesorter: new function() {

            var ts = this;

            ts.version = "2.10.8";

            ts.parsers = [];
            ts.widgets = [];
            ts.defaults = {

                // *** appearance
                theme            : 'default',  // adds tablesorter-{theme} to the table for styling
                widthFixed       : false,      // adds colgroup to fix widths of columns
                showProcessing   : false,      // show an indeterminate timer icon in the header when the table is sorted or filtered.

                headerTemplate   : '{content}',// header layout template (HTML ok); {content} = innerHTML, {icon} = <i/> (class from cssIcon)
                onRenderTemplate : null,       // function(index, template){ return template; }, (template is a string)
                onRenderHeader   : null,       // function(index){}, (nothing to return)

                // *** functionality
                cancelSelection  : true,       // prevent text selection in the header
                dateFormat       : 'mmddyyyy', // other options: "ddmmyyy" or "yyyymmdd"
                sortMultiSortKey : 'shiftKey', // key used to select additional columns
                sortResetKey     : 'ctrlKey',  // key used to remove sorting on a column
                usNumberFormat   : true,       // false for German "1.234.567,89" or French "1 234 567,89"
                delayInit        : false,      // if false, the parsed table contents will not update until the first sort
                serverSideSorting: false,      // if true, server-side sorting should be performed because client-side sorting will be disabled, but the ui and events will still be used.

                // *** sort options
                headers          : {},         // set sorter, string, empty, locked order, sortInitialOrder, filter, etc.
                ignoreCase       : true,       // ignore case while sorting
                sortForce        : null,       // column(s) first sorted; always applied
                sortList         : [],         // Initial sort order; applied initially; updated when manually sorted
                sortAppend       : null,       // column(s) sorted last; always applied

                sortInitialOrder : 'asc',      // sort direction on first click
                sortLocaleCompare: false,      // replace equivalent character (accented characters)
                sortReset        : false,      // third click on the header will reset column to default - unsorted
                sortRestart      : false,      // restart sort to "sortInitialOrder" when clicking on previously unsorted columns

                emptyTo          : 'bottom',   // sort empty cell to bottom, top, none, zero
                stringTo         : 'max',      // sort strings in numerical column as max, min, top, bottom, zero
                textExtraction   : 'simple',   // text extraction method/function - function(node, table, cellIndex){}
                textSorter       : null,       // use custom text sorter - function(a,b){ return a.sort(b); } // basic sort

                // *** widget options
                widgets: [],                   // method to add widgets, e.g. widgets: ['zebra']
                widgetOptions    : {
                    zebra : [ 'even', 'odd' ]    // zebra widget alternating row class names
                },
                initWidgets      : true,       // apply widgets on tablesorter initialization

                // *** callbacks
                initialized      : null,       // function(table){},

                // *** css class names
                tableClass       : 'tablesorter',
                cssAsc           : 'tablesorter-headerAsc',
                cssChildRow      : 'tablesorter-childRow', // previously "expand-child"
                cssDesc          : 'tablesorter-headerDesc',
                cssHeader        : 'tablesorter-header',
                cssHeaderRow     : 'tablesorter-headerRow',
                cssIcon          : 'tablesorter-icon', //  if this class exists, a <i> will be added to the header automatically
                cssInfoBlock     : 'tablesorter-infoOnly', // don't sort tbody with this class name
                cssProcessing    : 'tablesorter-processing', // processing icon applied to header during sort/filter

                // *** selectors
                selectorHeaders  : '> thead th, > thead td',
                selectorSort     : 'th, td',   // jQuery selector of content within selectorHeaders that is clickable to trigger a sort
                selectorRemove   : '.remove-me',

                // *** advanced
                debug            : false,

                // *** Internal variables
                headerList: [],
                empties: {},
                strings: {},
                parsers: []

                // deprecated; but retained for backwards compatibility
                // widgetZebra: { css: ["even", "odd"] }

            };

            /* debuging utils */
            function log(s) {
                if (typeof console !== "undefined" && typeof console.log !== "undefined") {
                    console.log(s);
                } else {
                    alert(s);
                }
            }

            function benchmark(s, d) {
                log(s + " (" + (new Date().getTime() - d.getTime()) + "ms)");
            }

            ts.log = log;
            ts.benchmark = benchmark;

            function getElementText(table, node, cellIndex) {
                if (!node) { return ""; }
                var c = table.config,
                    t = c.textExtraction, text = "";
                if (t === "simple") {
                    if (c.supportsTextContent) {
                        text = node.textContent; // newer browsers support this
                    } else {
                        text = $(node).text();
                    }
                } else {
                    if (typeof t === "function") {
                        text = t(node, table, cellIndex);
                    } else if (typeof t === "object" && t.hasOwnProperty(cellIndex)) {
                        text = t[cellIndex](node, table, cellIndex);
                    } else {
                        text = c.supportsTextContent ? node.textContent : $(node).text();
                    }
                }
                return $.trim(text);
            }

            function detectParserForColumn(table, rows, rowIndex, cellIndex) {
                var cur,
                    i = ts.parsers.length,
                    node = false,
                    nodeValue = '',
                    keepLooking = true;
                while (nodeValue === '' && keepLooking) {
                    rowIndex++;
                    if (rows[rowIndex]) {
                        node = rows[rowIndex].cells[cellIndex];
                        nodeValue = getElementText(table, node, cellIndex);
                        if (table.config.debug) {
                            log('Checking if value was empty on row ' + rowIndex + ', column: ' + cellIndex + ': "' + nodeValue + '"');
                        }
                    } else {
                        keepLooking = false;
                    }
                }
                while (--i >= 0) {
                    cur = ts.parsers[i];
                    // ignore the default text parser because it will always be true
                    if (cur && cur.id !== 'text' && cur.is && cur.is(nodeValue, table, node)) {
                        return cur;
                    }
                }
                // nothing found, return the generic parser (text)
                return ts.getParserById('text');
            }

            function buildParserCache(table) {
                var c = table.config,
                // update table bodies in case we start with an empty table
                    tb = c.$tbodies = c.$table.children('tbody:not(.' + c.cssInfoBlock + ')'),
                    rows, list, l, i, h, ch, p, parsersDebug = "";
                if ( tb.length === 0) {
                    return c.debug ? log('*Empty table!* Not building a parser cache') : '';
                }
                rows = tb[0].rows;
                if (rows[0]) {
                    list = [];
                    l = rows[0].cells.length;
                    for (i = 0; i < l; i++) {
                        // tons of thanks to AnthonyM1229 for working out the following selector (issue #74) to make this work in IE8!
                        // More fixes to this selector to work properly in iOS and jQuery 1.8+ (issue #132 & #174)
                        h = c.$headers.filter(':not([colspan])');
                        h = h.add( c.$headers.filter('[colspan="1"]') ) // ie8 fix
                            .filter('[data-column="' + i + '"]:last');
                        ch = c.headers[i];
                        // get column parser
                        p = ts.getParserById( ts.getData(h, ch, 'sorter') );
                        // empty cells behaviour - keeping emptyToBottom for backwards compatibility
                        c.empties[i] = ts.getData(h, ch, 'empty') || c.emptyTo || (c.emptyToBottom ? 'bottom' : 'top' );
                        // text strings behaviour in numerical sorts
                        c.strings[i] = ts.getData(h, ch, 'string') || c.stringTo || 'max';
                        if (!p) {
                            p = detectParserForColumn(table, rows, -1, i);
                        }
                        if (c.debug) {
                            parsersDebug += "column:" + i + "; parser:" + p.id + "; string:" + c.strings[i] + '; empty: ' + c.empties[i] + "\n";
                        }
                        list.push(p);
                    }
                }
                if (c.debug) {
                    log(parsersDebug);
                }
                c.parsers = list;
            }

            /* utils */
            function buildCache(table) {
                var b = table.tBodies,
                    tc = table.config,
                    totalRows,
                    totalCells,
                    parsers = tc.parsers,
                    t, v, i, j, k, c, cols, cacheTime, colMax = [];
                tc.cache = {};
                // if no parsers found, return - it's an empty table.
                if (!parsers) {
                    return tc.debug ? log('*Empty table!* Not building a cache') : '';
                }
                if (tc.debug) {
                    cacheTime = new Date();
                }
                // processing icon
                if (tc.showProcessing) {
                    ts.isProcessing(table, true);
                }
                for (k = 0; k < b.length; k++) {
                    tc.cache[k] = { row: [], normalized: [] };
                    // ignore tbodies with class name from css.cssInfoBlock
                    if (!$(b[k]).hasClass(tc.cssInfoBlock)) {
                        totalRows = (b[k] && b[k].rows.length) || 0;
                        totalCells = (b[k].rows[0] && b[k].rows[0].cells.length) || 0;
                        for (i = 0; i < totalRows; ++i) {
                            /** Add the table data to main data array */
                            c = $(b[k].rows[i]);
                            cols = [];
                            // if this is a child row, add it to the last row's children and continue to the next row
                            if (c.hasClass(tc.cssChildRow)) {
                                tc.cache[k].row[tc.cache[k].row.length - 1] = tc.cache[k].row[tc.cache[k].row.length - 1].add(c);
                                // go to the next for loop
                                continue;
                            }
                            tc.cache[k].row.push(c);
                            for (j = 0; j < totalCells; ++j) {
                                t = getElementText(table, c[0].cells[j], j);
                                // allow parsing if the string is empty, previously parsing would change it to zero,
                                // in case the parser needs to extract data from the table cell attributes
                                v = parsers[j].format(t, table, c[0].cells[j], j);
                                cols.push(v);
                                if ((parsers[j].type || '').toLowerCase() === "numeric") {
                                    colMax[j] = Math.max(Math.abs(v) || 0, colMax[j] || 0); // determine column max value (ignore sign)
                                }
                            }
                            cols.push(tc.cache[k].normalized.length); // add position for rowCache
                            tc.cache[k].normalized.push(cols);
                        }
                        tc.cache[k].colMax = colMax;
                    }
                }
                if (tc.showProcessing) {
                    ts.isProcessing(table); // remove processing icon
                }
                if (tc.debug) {
                    benchmark("Building cache for " + totalRows + " rows", cacheTime);
                }
            }

            // init flag (true) used by pager plugin to prevent widget application
            function appendToTable(table, init) {
                var c = table.config,
                    b = table.tBodies,
                    rows = [],
                    c2 = c.cache,
                    r, n, totalRows, checkCell, $bk, $tb,
                    i, j, k, l, pos, appendTime;
                if (!c2[0]) { return; } // empty table - fixes #206
                if (c.debug) {
                    appendTime = new Date();
                }
                for (k = 0; k < b.length; k++) {
                    $bk = $(b[k]);
                    if ($bk.length && !$bk.hasClass(c.cssInfoBlock)) {
                        // get tbody
                        $tb = ts.processTbody(table, $bk, true);
                        r = c2[k].row;
                        n = c2[k].normalized;
                        totalRows = n.length;
                        checkCell = totalRows ? (n[0].length - 1) : 0;
                        for (i = 0; i < totalRows; i++) {
                            pos = n[i][checkCell];
                            rows.push(r[pos]);
                            // removeRows used by the pager plugin
                            if (!c.appender || !c.removeRows) {
                                l = r[pos].length;
                                for (j = 0; j < l; j++) {
                                    $tb.append(r[pos][j]);
                                }
                            }
                        }
                        // restore tbody
                        ts.processTbody(table, $tb, false);
                    }
                }
                if (c.appender) {
                    c.appender(table, rows);
                }
                if (c.debug) {
                    benchmark("Rebuilt table", appendTime);
                }
                // apply table widgets
                if (!init) { ts.applyWidget(table); }
                // trigger sortend
                $(table).trigger("sortEnd", table);
            }

            // computeTableHeaderCellIndexes from:
            // http://www.javascripttoolbox.com/lib/table/examples.php
            // http://www.javascripttoolbox.com/temp/table_cellindex.html
            function computeThIndexes(t) {
                var matrix = [],
                    lookup = {},
                    cols = 0, // determine the number of columns
                    trs = $(t).find('thead:eq(0), tfoot').children('tr'), // children tr in tfoot - see issue #196
                    i, j, k, l, c, cells, rowIndex, cellId, rowSpan, colSpan, firstAvailCol, matrixrow;
                for (i = 0; i < trs.length; i++) {
                    cells = trs[i].cells;
                    for (j = 0; j < cells.length; j++) {
                        c = cells[j];
                        rowIndex = c.parentNode.rowIndex;
                        cellId = rowIndex + "-" + c.cellIndex;
                        rowSpan = c.rowSpan || 1;
                        colSpan = c.colSpan || 1;
                        if (typeof(matrix[rowIndex]) === "undefined") {
                            matrix[rowIndex] = [];
                        }
                        // Find first available column in the first row
                        for (k = 0; k < matrix[rowIndex].length + 1; k++) {
                            if (typeof(matrix[rowIndex][k]) === "undefined") {
                                firstAvailCol = k;
                                break;
                            }
                        }
                        lookup[cellId] = firstAvailCol;
                        cols = Math.max(firstAvailCol, cols);
                        // add data-column
                        $(c).attr({ 'data-column' : firstAvailCol }); // 'data-row' : rowIndex
                        for (k = rowIndex; k < rowIndex + rowSpan; k++) {
                            if (typeof(matrix[k]) === "undefined") {
                                matrix[k] = [];
                            }
                            matrixrow = matrix[k];
                            for (l = firstAvailCol; l < firstAvailCol + colSpan; l++) {
                                matrixrow[l] = "x";
                            }
                        }
                    }
                }
                t.config.columns = cols; // may not be accurate if # header columns !== # tbody columns
                return lookup;
            }

            function formatSortingOrder(v) {
                // look for "d" in "desc" order; return true
                return (/^d/i.test(v) || v === 1);
            }

            function buildHeaders(table) {
                var header_index = computeThIndexes(table), ch, $t,
                    h, i, t, lock, time, c = table.config;
                c.headerList = [];
                c.headerContent = [];
                if (c.debug) {
                    time = new Date();
                }
                i = c.cssIcon ? '<i class="' + c.cssIcon + '"></i>' : ''; // add icon if cssIcon option exists
                c.$headers = $(table).find(c.selectorHeaders).each(function(index) {
                    $t = $(this);
                    ch = c.headers[index];
                    c.headerContent[index] = this.innerHTML; // save original header content
                    // set up header template
                    t = c.headerTemplate.replace(/\{content\}/g, this.innerHTML).replace(/\{icon\}/g, i);
                    if (c.onRenderTemplate) {
                        h = c.onRenderTemplate.apply($t, [index, t]);
                        if (h && typeof h === 'string') { t = h; } // only change t if something is returned
                    }
                    this.innerHTML = '<div class="tablesorter-header-inner">' + t + '</div>'; // faster than wrapInner

                    if (c.onRenderHeader) { c.onRenderHeader.apply($t, [index]); }

                    this.column = header_index[this.parentNode.rowIndex + "-" + this.cellIndex];
                    this.order = formatSortingOrder( ts.getData($t, ch, 'sortInitialOrder') || c.sortInitialOrder ) ? [1,0,2] : [0,1,2];
                    this.count = -1; // set to -1 because clicking on the header automatically adds one
                    this.lockedOrder = false;
                    lock = ts.getData($t, ch, 'lockedOrder') || false;
                    if (typeof lock !== 'undefined' && lock !== false) {
                        this.order = this.lockedOrder = formatSortingOrder(lock) ? [1,1,1] : [0,0,0];
                    }
                    $t.addClass(c.cssHeader);
                    // add cell to headerList
                    c.headerList[index] = this;
                    // add to parent in case there are multiple rows
                    $t.parent().addClass(c.cssHeaderRow);
                    // allow keyboard cursor to focus on element
                    $t.attr("tabindex", 0);
                });
                // enable/disable sorting
                updateHeader(table);
                if (c.debug) {
                    benchmark("Built headers:", time);
                    log(c.$headers);
                }
            }

            function commonUpdate(table, resort, callback) {
                var c = table.config;
                // remove rows/elements before update
                c.$table.find(c.selectorRemove).remove();
                // rebuild parsers
                buildParserCache(table);
                // rebuild the cache map
                buildCache(table);
                checkResort(c.$table, resort, callback);
            }

            function updateHeader(table) {
                var s, c = table.config;
                c.$headers.each(function(index, th){
                    s = ts.getData( th, c.headers[index], 'sorter' ) === 'false';
                    th.sortDisabled = s;
                    $(th)[ s ? 'addClass' : 'removeClass' ]('sorter-false');
                });
            }

            function setHeadersCss(table) {
                var f, i, j, l,
                    c = table.config,
                    list = c.sortList,
                    css = [c.cssAsc, c.cssDesc],
                // find the footer
                    $t = $(table).find('tfoot tr').children().removeClass(css.join(' '));
                // remove all header information
                c.$headers.removeClass(css.join(' '));
                l = list.length;
                for (i = 0; i < l; i++) {
                    // direction = 2 means reset!
                    if (list[i][1] !== 2) {
                        // multicolumn sorting updating - choose the :last in case there are nested columns
                        f = c.$headers.not('.sorter-false').filter('[data-column="' + list[i][0] + '"]' + (l === 1 ? ':last' : '') );
                        if (f.length) {
                            for (j = 0; j < f.length; j++) {
                                if (!f[j].sortDisabled) {
                                    f.eq(j).addClass(css[list[i][1]]);
                                    // add sorted class to footer, if it exists
                                    if ($t.length) {
                                        $t.filter('[data-column="' + list[i][0] + '"]').eq(j).addClass(css[list[i][1]]);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // automatically add col group, and column sizes if set
            function fixColumnWidth(table) {
                if (table.config.widthFixed && $(table).find('colgroup').length === 0) {
                    var colgroup = $('<colgroup>'),
                        overallWidth = $(table).width();
                    $(table.tBodies[0]).find("tr:first").children("td").each(function() {
                        colgroup.append($('<col>').css('width', parseInt(($(this).width()/overallWidth)*1000, 10)/10 + '%'));
                    });
                    $(table).prepend(colgroup);
                }
            }

            function updateHeaderSortCount(table, list) {
                var s, t, o, c = table.config,
                    sl = list || c.sortList;
                c.sortList = [];
                $.each(sl, function(i,v){
                    // ensure all sortList values are numeric - fixes #127
                    s = [ parseInt(v[0], 10), parseInt(v[1], 10) ];
                    // make sure header exists
                    o = c.headerList[s[0]];
                    if (o) { // prevents error if sorton array is wrong
                        c.sortList.push(s);
                        t = $.inArray(s[1], o.order); // fixes issue #167
                        o.count = t >= 0 ? t : s[1] % (c.sortReset ? 3 : 2);
                    }
                });
            }

            function getCachedSortType(parsers, i) {
                return (parsers && parsers[i]) ? parsers[i].type || '' : '';
            }

            function initSort(table, cell, e){
                var a, i, j, o, s,
                    c = table.config,
                    k = !e[c.sortMultiSortKey],
                    $this = $(table);
                // Only call sortStart if sorting is enabled
                $this.trigger("sortStart", table);
                // get current column sort order
                cell.count = e[c.sortResetKey] ? 2 : (cell.count + 1) % (c.sortReset ? 3 : 2);
                // reset all sorts on non-current column - issue #30
                if (c.sortRestart) {
                    i = cell;
                    c.$headers.each(function() {
                        // only reset counts on columns that weren't just clicked on and if not included in a multisort
                        if (this !== i && (k || !$(this).is('.' + c.cssDesc + ',.' + c.cssAsc))) {
                            this.count = -1;
                        }
                    });
                }
                // get current column index
                i = cell.column;
                // user only wants to sort on one column
                if (k) {
                    // flush the sort list
                    c.sortList = [];
                    if (c.sortForce !== null) {
                        a = c.sortForce;
                        for (j = 0; j < a.length; j++) {
                            if (a[j][0] !== i) {
                                c.sortList.push(a[j]);
                            }
                        }
                    }
                    // add column to sort list
                    o = cell.order[cell.count];
                    if (o < 2) {
                        c.sortList.push([i, o]);
                        // add other columns if header spans across multiple
                        if (cell.colSpan > 1) {
                            for (j = 1; j < cell.colSpan; j++) {
                                c.sortList.push([i + j, o]);
                            }
                        }
                    }
                    // multi column sorting
                } else {
                    // get rid of the sortAppend before adding more - fixes issue #115
                    if (c.sortAppend && c.sortList.length > 1) {
                        if (ts.isValueInArray(c.sortAppend[0][0], c.sortList)) {
                            c.sortList.pop();
                        }
                    }
                    // the user has clicked on an already sorted column
                    if (ts.isValueInArray(i, c.sortList)) {
                        // reverse the sorting direction for all tables
                        for (j = 0; j < c.sortList.length; j++) {
                            s = c.sortList[j];
                            o = c.headerList[s[0]];
                            if (s[0] === i) {
                                s[1] = o.order[o.count];
                                if (s[1] === 2) {
                                    c.sortList.splice(j,1);
                                    o.count = -1;
                                }
                            }
                        }
                    } else {
                        // add column to sort list array
                        o = cell.order[cell.count];
                        if (o < 2) {
                            c.sortList.push([i, o]);
                            // add other columns if header spans across multiple
                            if (cell.colSpan > 1) {
                                for (j = 1; j < cell.colSpan; j++) {
                                    c.sortList.push([i + j, o]);
                                }
                            }
                        }
                    }
                }
                if (c.sortAppend !== null) {
                    a = c.sortAppend;
                    for (j = 0; j < a.length; j++) {
                        if (a[j][0] !== i) {
                            c.sortList.push(a[j]);
                        }
                    }
                }
                // sortBegin event triggered immediately before the sort
                $this.trigger("sortBegin", table);
                // setTimeout needed so the processing icon shows up
                setTimeout(function(){
                    // set css for headers
                    setHeadersCss(table);
                    multisort(table);
                    appendToTable(table);
                }, 1);
            }

            // sort multiple columns
            function multisort(table) { /*jshint loopfunc:true */
                var dir = 0, tc = table.config,
                    sortList = tc.sortList, l = sortList.length, bl = table.tBodies.length,
                    sortTime, i, k, c, colMax, cache, lc, s, order, orgOrderCol;
                if (tc.serverSideSorting || !tc.cache[0]) { // empty table - fixes #206
                    return;
                }
                if (tc.debug) { sortTime = new Date(); }
                for (k = 0; k < bl; k++) {
                    colMax = tc.cache[k].colMax;
                    cache = tc.cache[k].normalized;
                    lc = cache.length;
                    orgOrderCol = (cache && cache[0]) ? cache[0].length - 1 : 0;
                    cache.sort(function(a, b) {
                        // cache is undefined here in IE, so don't use it!
                        for (i = 0; i < l; i++) {
                            c = sortList[i][0];
                            order = sortList[i][1];
                            // fallback to natural sort since it is more robust
                            s = /n/i.test(getCachedSortType(tc.parsers, c)) ? "Numeric" : "Text";
                            s += order === 0 ? "" : "Desc";
                            if (/Numeric/.test(s) && tc.strings[c]) {
                                // sort strings in numerical columns
                                if (typeof (tc.string[tc.strings[c]]) === 'boolean') {
                                    dir = (order === 0 ? 1 : -1) * (tc.string[tc.strings[c]] ? -1 : 1);
                                } else {
                                    dir = (tc.strings[c]) ? tc.string[tc.strings[c]] || 0 : 0;
                                }
                            }
                            var sort = $.tablesorter["sort" + s](table, a[c], b[c], c, colMax[c], dir);
                            if (sort) { return sort; }
                        }
                        return a[orgOrderCol] - b[orgOrderCol];
                    });
                }
                if (tc.debug) { benchmark("Sorting on " + sortList.toString() + " and dir " + order + " time", sortTime); }
            }

            function resortComplete($table, callback){
                $table.trigger('updateComplete');
                if (typeof callback === "function") {
                    callback($table[0]);
                }
            }

            function checkResort($table, flag, callback) {
                // don't try to resort if the table is still processing
                // this will catch spamming of the updateCell method
                if (flag !== false && !$table[0].isProcessing) {
                    $table.trigger("sorton", [$table[0].config.sortList, function(){
                        resortComplete($table, callback);
                    }]);
                } else {
                    resortComplete($table, callback);
                }
            }

            function bindEvents(table){
                var c = table.config,
                    $this = c.$table,
                    j, downTime;
                // apply event handling to headers
                c.$headers
                    // http://stackoverflow.com/questions/5312849/jquery-find-self;
                    .find(c.selectorSort).add( c.$headers.filter(c.selectorSort) )
                    .unbind('mousedown.tablesorter mouseup.tablesorter sort.tablesorter keypress.tablesorter')
                    .bind('mousedown.tablesorter mouseup.tablesorter sort.tablesorter keypress.tablesorter', function(e, external) {
                        // only recognize left clicks or enter
                        if ( ((e.which || e.button) !== 1 && !/sort|keypress/.test(e.type)) || (e.type === 'keypress' && e.which !== 13) ) {
                            return false;
                        }
                        // ignore long clicks (prevents resizable widget from initializing a sort)
                        if (e.type === 'mouseup' && external !== true && (new Date().getTime() - downTime > 250)) { return false; }
                        // set timer on mousedown
                        if (e.type === 'mousedown') {
                            downTime = new Date().getTime();
                            return e.target.tagName === "INPUT" ? '' : !c.cancelSelection;
                        }
                        if (c.delayInit && !c.cache) { buildCache(table); }
                        // jQuery v1.2.6 doesn't have closest()
                        var $cell = /TH|TD/.test(this.tagName) ? $(this) : $(this).parents('th, td').filter(':first'), cell = $cell[0];
                        if (!cell.sortDisabled) {
                            initSort(table, cell, e);
                        }
                    });
                if (c.cancelSelection) {
                    // cancel selection
                    c.$headers
                        .attr('unselectable', 'on')
                        .bind('selectstart', false)
                        .css({
                            'user-select': 'none',
                            'MozUserSelect': 'none' // not needed for jQuery 1.8+
                        });
                }
                // apply easy methods that trigger bound events
                $this
                    .unbind('sortReset update updateRows updateCell updateAll addRows sorton appendCache applyWidgetId applyWidgets refreshWidgets destroy mouseup mouseleave '.split(' ').join('.tablesorter '))
                    .bind("sortReset.tablesorter", function(e){
                        e.stopPropagation();
                        c.sortList = [];
                        setHeadersCss(table);
                        multisort(table);
                        appendToTable(table);
                    })
                    .bind("updateAll.tablesorter", function(e, resort, callback){
                        e.stopPropagation();
                        ts.refreshWidgets(table, true, true);
                        ts.restoreHeaders(table);
                        buildHeaders(table);
                        bindEvents(table);
                        commonUpdate(table, resort, callback);
                    })
                    .bind("update.tablesorter updateRows.tablesorter", function(e, resort, callback) {
                        e.stopPropagation();
                        // update sorting (if enabled/disabled)
                        updateHeader(table);
                        commonUpdate(table, resort, callback);
                    })
                    .bind("updateCell.tablesorter", function(e, cell, resort, callback) {
                        e.stopPropagation();
                        $this.find(c.selectorRemove).remove();
                        // get position from the dom
                        var l, row, icell,
                            $tb = $this.find('tbody'),
                        // update cache - format: function(s, table, cell, cellIndex)
                        // no closest in jQuery v1.2.6 - tbdy = $tb.index( $(cell).closest('tbody') ),$row = $(cell).closest('tr');
                            tbdy = $tb.index( $(cell).parents('tbody').filter(':first') ),
                            $row = $(cell).parents('tr').filter(':first');
                        cell = $(cell)[0]; // in case cell is a jQuery object
                        // tbody may not exist if update is initialized while tbody is removed for processing
                        if ($tb.length && tbdy >= 0) {
                            row = $tb.eq(tbdy).find('tr').index( $row );
                            icell = cell.cellIndex;
                            l = c.cache[tbdy].normalized[row].length - 1;
                            c.cache[tbdy].row[table.config.cache[tbdy].normalized[row][l]] = $row;
                            c.cache[tbdy].normalized[row][icell] = c.parsers[icell].format( getElementText(table, cell, icell), table, cell, icell );
                            checkResort($this, resort, callback);
                        }
                    })
                    .bind("addRows.tablesorter", function(e, $row, resort, callback) {
                        e.stopPropagation();
                        var i, rows = $row.filter('tr').length,
                            dat = [], l = $row[0].cells.length,
                            tbdy = $this.find('tbody').index( $row.parents('tbody').filter(':first') );
                        // fixes adding rows to an empty table - see issue #179
                        if (!c.parsers) {
                            buildParserCache(table);
                        }
                        // add each row
                        for (i = 0; i < rows; i++) {
                            // add each cell
                            for (j = 0; j < l; j++) {
                                dat[j] = c.parsers[j].format( getElementText(table, $row[i].cells[j], j), table, $row[i].cells[j], j );
                            }
                            // add the row index to the end
                            dat.push(c.cache[tbdy].row.length);
                            // update cache
                            c.cache[tbdy].row.push([$row[i]]);
                            c.cache[tbdy].normalized.push(dat);
                            dat = [];
                        }
                        // resort using current settings
                        checkResort($this, resort, callback);
                    })
                    .bind("sorton.tablesorter", function(e, list, callback, init) {
                        e.stopPropagation();
                        $this.trigger("sortStart", this);
                        // update header count index
                        updateHeaderSortCount(table, list);
                        // set css for headers
                        setHeadersCss(table);
                        $this.trigger("sortBegin", this);
                        // sort the table and append it to the dom
                        multisort(table);
                        appendToTable(table, init);
                        if (typeof callback === "function") {
                            callback(table);
                        }
                    })
                    .bind("appendCache.tablesorter", function(e, callback, init) {
                        e.stopPropagation();
                        appendToTable(table, init);
                        if (typeof callback === "function") {
                            callback(table);
                        }
                    })
                    .bind("applyWidgetId.tablesorter", function(e, id) {
                        e.stopPropagation();
                        ts.getWidgetById(id).format(table, c, c.widgetOptions);
                    })
                    .bind("applyWidgets.tablesorter", function(e, init) {
                        e.stopPropagation();
                        // apply widgets
                        ts.applyWidget(table, init);
                    })
                    .bind("refreshWidgets.tablesorter", function(e, all, dontapply){
                        e.stopPropagation();
                        ts.refreshWidgets(table, all, dontapply);
                    })
                    .bind("destroy.tablesorter", function(e, c, cb){
                        e.stopPropagation();
                        ts.destroy(table, c, cb);
                    });
            }

            /* public methods */
            ts.construct = function(settings) {
                return this.each(function() {
                    // if no thead or tbody, or tablesorter is already present, quit
                    if (!this.tHead || this.tBodies.length === 0 || this.hasInitialized === true) {
                        return (this.config && this.config.debug) ? log('stopping initialization! No thead, tbody or tablesorter has already been initialized') : '';
                    }
                    // declare
                    var $this = $(this), table = this,
                        c, k = '',
                        m = $.metadata;
                    // initialization flag
                    table.hasInitialized = false;
                    // table is being processed flag
                    table.isProcessing = true;
                    // new blank config object
                    table.config = {};
                    // merge and extend
                    c = $.extend(true, table.config, ts.defaults, settings);
                    // save the settings where they read
                    $.data(table, "tablesorter", c);
                    if (c.debug) { $.data( table, 'startoveralltimer', new Date()); }
                    // constants
                    c.supportsTextContent = $('<span>x</span>')[0].textContent === 'x';
                    c.supportsDataObject = parseFloat($.fn.jquery) >= 1.4;
                    // digit sort text location; keeping max+/- for backwards compatibility
                    c.string = { 'max': 1, 'min': -1, 'max+': 1, 'max-': -1, 'zero': 0, 'none': 0, 'null': 0, 'top': true, 'bottom': false };
                    // add table theme class only if there isn't already one there
                    if (!/tablesorter\-/.test($this.attr('class'))) {
                        k = (c.theme !== '' ? ' tablesorter-' + c.theme : '');
                    }
                    c.$table = $this.addClass(c.tableClass + k);
                    c.$tbodies = $this.children('tbody:not(.' + c.cssInfoBlock + ')');
                    // build headers
                    buildHeaders(table);
                    // fixate columns if the users supplies the fixedWidth option
                    // do this after theme has been applied
                    fixColumnWidth(table);
                    // try to auto detect column type, and store in tables config
                    buildParserCache(table);
                    // build the cache for the tbody cells
                    // delayInit will delay building the cache until the user starts a sort
                    if (!c.delayInit) { buildCache(table); }
                    // bind all header events and methods
                    bindEvents(table);
                    // get sort list from jQuery data or metadata
                    // in jQuery < 1.4, an error occurs when calling $this.data()
                    if (c.supportsDataObject && typeof $this.data().sortlist !== 'undefined') {
                        c.sortList = $this.data().sortlist;
                    } else if (m && ($this.metadata() && $this.metadata().sortlist)) {
                        c.sortList = $this.metadata().sortlist;
                    }
                    // apply widget init code
                    ts.applyWidget(table, true);
                    // if user has supplied a sort list to constructor
                    if (c.sortList.length > 0) {
                        $this.trigger("sorton", [c.sortList, {}, !c.initWidgets]);
                    } else if (c.initWidgets) {
                        // apply widget format
                        ts.applyWidget(table);
                    }

                    // show processesing icon
                    if (c.showProcessing) {
                        $this
                            .unbind('sortBegin.tablesorter sortEnd.tablesorter')
                            .bind('sortBegin.tablesorter sortEnd.tablesorter', function(e) {
                                ts.isProcessing(table, e.type === 'sortBegin');
                            });
                    }

                    // initialized
                    table.hasInitialized = true;
                    table.isProcessing = false;
                    if (c.debug) {
                        ts.benchmark("Overall initialization time", $.data( table, 'startoveralltimer'));
                    }
                    $this.trigger('tablesorter-initialized', table);
                    if (typeof c.initialized === 'function') { c.initialized(table); }
                });
            };

            // *** Process table ***
            // add processing indicator
            ts.isProcessing = function(table, toggle, $ths) {
                table = $(table);
                var c = table[0].config,
                // default to all headers
                    $h = $ths || table.find('.' + c.cssHeader);
                if (toggle) {
                    if (c.sortList.length > 0) {
                        // get headers from the sortList
                        $h = $h.filter(function(){
                            // get data-column from attr to keep  compatibility with jQuery 1.2.6
                            return this.sortDisabled ? false : ts.isValueInArray( parseFloat($(this).attr('data-column')), c.sortList);
                        });
                    }
                    $h.addClass(c.cssProcessing);
                } else {
                    $h.removeClass(c.cssProcessing);
                }
            };

            // detach tbody but save the position
            // don't use tbody because there are portions that look for a tbody index (updateCell)
            ts.processTbody = function(table, $tb, getIt){
                var holdr;
                if (getIt) {
                    table.isProcessing = true;
                    $tb.before('<span class="tablesorter-savemyplace"/>');
                    holdr = ($.fn.detach) ? $tb.detach() : $tb.remove();
                    return holdr;
                }
                holdr = $(table).find('span.tablesorter-savemyplace');
                $tb.insertAfter( holdr );
                holdr.remove();
                table.isProcessing = false;
            };

            ts.clearTableBody = function(table) {
                $(table)[0].config.$tbodies.empty();
            };

            // restore headers
            ts.restoreHeaders = function(table){
                var c = table.config;
                // don't use c.$headers here in case header cells were swapped
                c.$table.find(c.selectorHeaders).each(function(i){
                    // only restore header cells if it is wrapped
                    // because this is also used by the updateAll method
                    if ($(this).find('.tablesorter-header-inner').length){
                        $(this).html( c.headerContent[i] );
                    }
                });
            };

            ts.destroy = function(table, removeClasses, callback){
                table = $(table)[0];
                if (!table.hasInitialized) { return; }
                // remove all widgets
                ts.refreshWidgets(table, true, true);
                var $t = $(table), c = table.config,
                    $h = $t.find('thead:first'),
                    $r = $h.find('tr.' + c.cssHeaderRow).removeClass(c.cssHeaderRow),
                    $f = $t.find('tfoot:first > tr').children('th, td');
                // remove widget added rows, just in case
                $h.find('tr').not($r).remove();
                // disable tablesorter
                $t
                    .removeData('tablesorter')
                    .unbind('sortReset update updateAll updateRows updateCell addRows sorton appendCache applyWidgetId applyWidgets refreshWidgets destroy mouseup mouseleave keypress sortBegin sortEnd '.split(' ').join('.tablesorter '));
                c.$headers.add($f)
                    .removeClass(c.cssHeader + ' ' + c.cssAsc + ' ' + c.cssDesc)
                    .removeAttr('data-column');
                $r.find(c.selectorSort).unbind('mousedown.tablesorter mouseup.tablesorter keypress.tablesorter');
                ts.restoreHeaders(table);
                if (removeClasses !== false) {
                    $t.removeClass(c.tableClass + ' tablesorter-' + c.theme);
                }
                // clear flag in case the plugin is initialized again
                table.hasInitialized = false;
                if (typeof callback === 'function') {
                    callback(table);
                }
            };

            // *** sort functions ***
            // regex used in natural sort
            ts.regex = [
                /(^([+\-]?(?:0|[1-9]\d*)(?:\.\d*)?(?:[eE][+\-]?\d+)?)?$|^0x[0-9a-f]+$|\d+)/gi, // chunk/tokenize numbers & letters
                /(^([\w ]+,?[\w ]+)?[\w ]+,?[\w ]+\d+:\d+(:\d+)?[\w ]?|^\d{1,4}[\/\-]\d{1,4}[\/\-]\d{1,4}|^\w+, \w+ \d+, \d{4})/, //date
                /^0x[0-9a-f]+$/i // hex
            ];

            // Natural sort - https://github.com/overset/javascript-natural-sort
            ts.sortText = function(table, a, b, col) {
                if (a === b) { return 0; }
                var c = table.config, e = c.string[ (c.empties[col] || c.emptyTo ) ],
                    r = ts.regex, xN, xD, yN, yD, xF, yF, i, mx;
                if (a === '' && e !== 0) { return typeof e === 'boolean' ? (e ? -1 : 1) : -e || -1; }
                if (b === '' && e !== 0) { return typeof e === 'boolean' ? (e ? 1 : -1) : e || 1; }
                if (typeof c.textSorter === 'function') { return c.textSorter(a, b, table, col); }
                // chunk/tokenize
                xN = a.replace(r[0], '\\0$1\\0').replace(/\\0$/, '').replace(/^\\0/, '').split('\\0');
                yN = b.replace(r[0], '\\0$1\\0').replace(/\\0$/, '').replace(/^\\0/, '').split('\\0');
                // numeric, hex or date detection
                xD = parseInt(a.match(r[2]),16) || (xN.length !== 1 && a.match(r[1]) && Date.parse(a));
                yD = parseInt(b.match(r[2]),16) || (xD && b.match(r[1]) && Date.parse(b)) || null;
                // first try and sort Hex codes or Dates
                if (yD) {
                    if ( xD < yD ) { return -1; }
                    if ( xD > yD ) { return 1; }
                }
                mx = Math.max(xN.length, yN.length);
                // natural sorting through split numeric strings and default strings
                for (i = 0; i < mx; i++) {
                    // find floats not starting with '0', string or 0 if not defined
                    xF = isNaN(xN[i]) ? xN[i] || 0 : parseFloat(xN[i]) || 0;
                    yF = isNaN(yN[i]) ? yN[i] || 0 : parseFloat(yN[i]) || 0;
                    // handle numeric vs string comparison - number < string - (Kyle Adams)
                    if (isNaN(xF) !== isNaN(yF)) { return (isNaN(xF)) ? 1 : -1; }
                    // rely on string comparison if different types - i.e. '02' < 2 != '02' < '2'
                    if (typeof xF !== typeof yF) {
                        xF += '';
                        yF += '';
                    }
                    if (xF < yF) { return -1; }
                    if (xF > yF) { return 1; }
                }
                return 0;
            };

            ts.sortTextDesc = function(table, a, b, col) {
                if (a === b) { return 0; }
                var c = table.config, e = c.string[ (c.empties[col] || c.emptyTo ) ];
                if (a === '' && e !== 0) { return typeof e === 'boolean' ? (e ? -1 : 1) : e || 1; }
                if (b === '' && e !== 0) { return typeof e === 'boolean' ? (e ? 1 : -1) : -e || -1; }
                if (typeof c.textSorter === 'function') { return c.textSorter(b, a, table, col); }
                return ts.sortText(table, b, a);
            };

            // return text string value by adding up ascii value
            // so the text is somewhat sorted when using a digital sort
            // this is NOT an alphanumeric sort
            ts.getTextValue = function(a, mx, d) {
                if (mx) {
                    // make sure the text value is greater than the max numerical value (mx)
                    var i, l = a ? a.length : 0, n = mx + d;
                    for (i = 0; i < l; i++) {
                        n += a.charCodeAt(i);
                    }
                    return d * n;
                }
                return 0;
            };

            ts.sortNumeric = function(table, a, b, col, mx, d) {
                if (a === b) { return 0; }
                var c = table.config, e = c.string[ (c.empties[col] || c.emptyTo ) ];
                if (a === '' && e !== 0) { return typeof e === 'boolean' ? (e ? -1 : 1) : -e || -1; }
                if (b === '' && e !== 0) { return typeof e === 'boolean' ? (e ? 1 : -1) : e || 1; }
                if (isNaN(a)) { a = ts.getTextValue(a, mx, d); }
                if (isNaN(b)) { b = ts.getTextValue(b, mx, d); }
                return a - b;
            };

            ts.sortNumericDesc = function(table, a, b, col, mx, d) {
                if (a === b) { return 0; }
                var c = table.config, e = c.string[ (c.empties[col] || c.emptyTo ) ];
                if (a === '' && e !== 0) { return typeof e === 'boolean' ? (e ? -1 : 1) : e || 1; }
                if (b === '' && e !== 0) { return typeof e === 'boolean' ? (e ? 1 : -1) : -e || -1; }
                if (isNaN(a)) { a = ts.getTextValue(a, mx, d); }
                if (isNaN(b)) { b = ts.getTextValue(b, mx, d); }
                return b - a;
            };

            // used when replacing accented characters during sorting
            ts.characterEquivalents = {
                "a" : "\u00e1\u00e0\u00e2\u00e3\u00e4\u0105\u00e5", // áàâãäąå
                "A" : "\u00c1\u00c0\u00c2\u00c3\u00c4\u0104\u00c5", // ÁÀÂÃÄĄÅ
                "c" : "\u00e7\u0107\u010d", // çćč
                "C" : "\u00c7\u0106\u010c", // ÇĆČ
                "e" : "\u00e9\u00e8\u00ea\u00eb\u011b\u0119", // éèêëěę
                "E" : "\u00c9\u00c8\u00ca\u00cb\u011a\u0118", // ÉÈÊËĚĘ
                "i" : "\u00ed\u00ec\u0130\u00ee\u00ef\u0131", // íìİîïı
                "I" : "\u00cd\u00cc\u0130\u00ce\u00cf", // ÍÌİÎÏ
                "o" : "\u00f3\u00f2\u00f4\u00f5\u00f6", // óòôõö
                "O" : "\u00d3\u00d2\u00d4\u00d5\u00d6", // ÓÒÔÕÖ
                "ss": "\u00df", // ß (s sharp)
                "SS": "\u1e9e", // ẞ (Capital sharp s)
                "u" : "\u00fa\u00f9\u00fb\u00fc\u016f", // úùûüů
                "U" : "\u00da\u00d9\u00db\u00dc\u016e" // ÚÙÛÜŮ
            };
            ts.replaceAccents = function(s) {
                var a, acc = '[', eq = ts.characterEquivalents;
                if (!ts.characterRegex) {
                    ts.characterRegexArray = {};
                    for (a in eq) {
                        if (typeof a === 'string') {
                            acc += eq[a];
                            ts.characterRegexArray[a] = new RegExp('[' + eq[a] + ']', 'g');
                        }
                    }
                    ts.characterRegex = new RegExp(acc + ']');
                }
                if (ts.characterRegex.test(s)) {
                    for (a in eq) {
                        if (typeof a === 'string') {
                            s = s.replace( ts.characterRegexArray[a], a );
                        }
                    }
                }
                return s;
            };

            // *** utilities ***
            ts.isValueInArray = function(v, a) {
                var i, l = a.length;
                for (i = 0; i < l; i++) {
                    if (a[i][0] === v) {
                        return true;
                    }
                }
                return false;
            };

            ts.addParser = function(parser) {
                var i, l = ts.parsers.length, a = true;
                for (i = 0; i < l; i++) {
                    if (ts.parsers[i].id.toLowerCase() === parser.id.toLowerCase()) {
                        a = false;
                    }
                }
                if (a) {
                    ts.parsers.push(parser);
                }
            };

            ts.getParserById = function(name) {
                var i, l = ts.parsers.length;
                for (i = 0; i < l; i++) {
                    if (ts.parsers[i].id.toLowerCase() === (name.toString()).toLowerCase()) {
                        return ts.parsers[i];
                    }
                }
                return false;
            };

            ts.addWidget = function(widget) {
                ts.widgets.push(widget);
            };

            ts.getWidgetById = function(name) {
                var i, w, l = ts.widgets.length;
                for (i = 0; i < l; i++) {
                    w = ts.widgets[i];
                    if (w && w.hasOwnProperty('id') && w.id.toLowerCase() === name.toLowerCase()) {
                        return w;
                    }
                }
            };

            ts.applyWidget = function(table, init) {
                table = $(table)[0]; // in case this is called externally
                var c = table.config,
                    wo = c.widgetOptions,
                    widgets = [],
                    time, i, w, wd;
                if (c.debug) { time = new Date(); }
                if (c.widgets.length) {
                    // ensure unique widget ids
                    c.widgets = $.grep(c.widgets, function(v, k){
                        return $.inArray(v, c.widgets) === k;
                    });
                    // build widget array & add priority as needed
                    $.each(c.widgets || [], function(i,n){
                        wd = ts.getWidgetById(n);
                        if (wd && wd.id) {
                            // set priority to 10 if not defined
                            if (!wd.priority) { wd.priority = 10; }
                            widgets[i] = wd;
                        }
                    });
                    // sort widgets by priority
                    widgets.sort(function(a, b){
                        return a.priority < b.priority ? -1 : a.priority === b.priority ? 0 : 1;
                    });

                    // add/update selected widgets
                    $.each(widgets, function(i,w){
                        if (w) {
                            if (init) {
                                if (w.hasOwnProperty('options')) {
                                    wo = table.config.widgetOptions = $.extend( true, {}, w.options, wo );
                                }
                                if (w.hasOwnProperty('init')) {
                                    w.init(table, w, c, wo);
                                }
                            } else if (!init && w.hasOwnProperty('format')) {
                                w.format(table, c, wo, false);
                            }
                        }
                    });
                }
                if (c.debug) {
                    w = c.widgets.length;
                    benchmark("Completed " + (init === true ? "initializing " : "applying ") + w + " widget" + (w !== 1 ? "s" : ""), time);
                }
            };

            ts.refreshWidgets = function(table, doAll, dontapply) {
                table = $(table)[0]; // see issue #243
                var i, c = table.config,
                    cw = c.widgets,
                    w = ts.widgets, l = w.length;
                // remove previous widgets
                for (i = 0; i < l; i++){
                    if ( w[i] && w[i].id && (doAll || $.inArray( w[i].id, cw ) < 0) ) {
                        if (c.debug) { log( 'Refeshing widgets: Removing ' + w[i].id  ); }
                        if (w[i].hasOwnProperty('remove')) { w[i].remove(table, c, c.widgetOptions); }
                    }
                }
                if (dontapply !== true) {
                    ts.applyWidget(table, doAll);
                }
            };

            // get sorter, string, empty, etc options for each column from
            // jQuery data, metadata, header option or header class name ("sorter-false")
            // priority = jQuery data > meta > headers option > header class name
            ts.getData = function(h, ch, key) {
                var val = '', $h = $(h), m, cl;
                if (!$h.length) { return ''; }
                m = $.metadata ? $h.metadata() : false;
                cl = ' ' + ($h.attr('class') || '');
                if (typeof $h.data(key) !== 'undefined' || typeof $h.data(key.toLowerCase()) !== 'undefined'){
                    // "data-lockedOrder" is assigned to "lockedorder"; but "data-locked-order" is assigned to "lockedOrder"
                    // "data-sort-initial-order" is assigned to "sortInitialOrder"
                    val += $h.data(key) || $h.data(key.toLowerCase());
                } else if (m && typeof m[key] !== 'undefined') {
                    val += m[key];
                } else if (ch && typeof ch[key] !== 'undefined') {
                    val += ch[key];
                } else if (cl !== ' ' && cl.match(' ' + key + '-')) {
                    // include sorter class name "sorter-text", etc; now works with "sorter-my-custom-parser"
                    val = cl.match( new RegExp('\\s' + key + '-([\\w-]+)') )[1] || '';
                }
                return $.trim(val);
            };

            ts.formatFloat = function(s, table) {
                if (typeof s !== 'string' || s === '') { return s; }
                // allow using formatFloat without a table; defaults to US number format
                var i,
                    t = table && table.config ? table.config.usNumberFormat !== false :
                        typeof table !== "undefined" ? table : true;
                if (t) {
                    // US Format - 1,234,567.89 -> 1234567.89
                    s = s.replace(/,/g,'');
                } else {
                    // German Format = 1.234.567,89 -> 1234567.89
                    // French Format = 1 234 567,89 -> 1234567.89
                    s = s.replace(/[\s|\.]/g,'').replace(/,/g,'.');
                }
                if(/^\s*\([.\d]+\)/.test(s)) {
                    // make (#) into a negative number -> (10) = -10
                    s = s.replace(/^\s*\(/,'-').replace(/\)/,'');
                }
                i = parseFloat(s);
                // return the text instead of zero
                return isNaN(i) ? $.trim(s) : i;
            };

            ts.isDigit = function(s) {
                // replace all unwanted chars and match
                return isNaN(s) ? (/^[\-+(]?\d+[)]?$/).test(s.toString().replace(/[,.'"\s]/g, '')) : true;
            };

        }()
    });

    // make shortcut
    var ts = $.tablesorter;

    // extend plugin scope
    $.fn.extend({
        tablesorter: ts.construct
    });

    // add default parsers
    ts.addParser({
        id: "text",
        is: function() {
            return true;
        },
        format: function(s, table) {
            var c = table.config;
            if (s) {
                s = $.trim( c.ignoreCase ? s.toLocaleLowerCase() : s );
                s = c.sortLocaleCompare ? ts.replaceAccents(s) : s;
            }
            return s;
        },
        type: "text"
    });

    ts.addParser({
        id: "digit",
        is: function(s) {
            return ts.isDigit(s);
        },
        format: function(s, table) {
            var n = ts.formatFloat((s || '').replace(/[^\w,. \-()]/g, ""), table);
            return s && typeof n === 'number' ? n : s ? $.trim( s && table.config.ignoreCase ? s.toLocaleLowerCase() : s ) : s;
        },
        type: "numeric"
    });

    ts.addParser({
        id: "currency",
        is: function(s) {
            return (/^\(?\d+[\u00a3$\u20ac\u00a4\u00a5\u00a2?.]|[\u00a3$\u20ac\u00a4\u00a5\u00a2?.]\d+\)?$/).test((s || '').replace(/[,. ]/g,'')); // £$€¤¥¢
        },
        format: function(s, table) {
            var n = ts.formatFloat((s || '').replace(/[^\w,. \-()]/g, ""), table);
            return s && typeof n === 'number' ? n : s ? $.trim( s && table.config.ignoreCase ? s.toLocaleLowerCase() : s ) : s;
        },
        type: "numeric"
    });

    ts.addParser({
        id: "ipAddress",
        is: function(s) {
            return (/^\d{1,3}[\.]\d{1,3}[\.]\d{1,3}[\.]\d{1,3}$/).test(s);
        },
        format: function(s, table) {
            var i, a = s ? s.split(".") : '',
                r = "",
                l = a.length;
            for (i = 0; i < l; i++) {
                r += ("00" + a[i]).slice(-3);
            }
            return s ? ts.formatFloat(r, table) : s;
        },
        type: "numeric"
    });

    ts.addParser({
        id: "url",
        is: function(s) {
            return (/^(https?|ftp|file):\/\//).test(s);
        },
        format: function(s) {
            return s ? $.trim(s.replace(/(https?|ftp|file):\/\//, '')) : s;
        },
        type: "text"
    });

    ts.addParser({
        id: "isoDate",
        is: function(s) {
            return (/^\d{4}[\/\-]\d{1,2}[\/\-]\d{1,2}/).test(s);
        },
        format: function(s, table) {
            return s ? ts.formatFloat((s !== "") ? (new Date(s.replace(/-/g, "/")).getTime() || "") : "", table) : s;
        },
        type: "numeric"
    });

    ts.addParser({
        id: "percent",
        is: function(s) {
            return (/(\d\s*?%|%\s*?\d)/).test(s) && s.length < 15;
        },
        format: function(s, table) {
            return s ? ts.formatFloat(s.replace(/%/g, ""), table) : s;
        },
        type: "numeric"
    });

    ts.addParser({
        id: "usLongDate",
        is: function(s) {
            // two digit years are not allowed cross-browser
            // Jan 01, 2013 12:34:56 PM or 01 Jan 2013
            return (/^[A-Z]{3,10}\.?\s+\d{1,2},?\s+(\d{4})(\s+\d{1,2}:\d{2}(:\d{2})?(\s+[AP]M)?)?$/i).test(s) || (/^\d{1,2}\s+[A-Z]{3,10}\s+\d{4}/i).test(s);
        },
        format: function(s, table) {
            return s ? ts.formatFloat( (new Date(s.replace(/(\S)([AP]M)$/i, "$1 $2")).getTime() || ''), table) : s;
        },
        type: "numeric"
    });

    ts.addParser({
        id: "shortDate", // "mmddyyyy", "ddmmyyyy" or "yyyymmdd"
        is: function(s) {
            // testing for ##-##-#### or ####-##-##, so it's not perfect; time can be included
            return (/(^\d{1,2}[\/\s]\d{1,2}[\/\s]\d{4})|(^\d{4}[\/\s]\d{1,2}[\/\s]\d{1,2})/).test((s || '').replace(/\s+/g," ").replace(/[\-.,]/g, "/"));
        },
        format: function(s, table, cell, cellIndex) {
            if (s) {
                var c = table.config, ci = c.headerList[cellIndex],
                    format = ci.dateFormat || ts.getData( ci, c.headers[cellIndex], 'dateFormat') || c.dateFormat;
                s = s.replace(/\s+/g," ").replace(/[\-.,]/g, "/"); // escaped - because JSHint in Firefox was showing it as an error
                if (format === "mmddyyyy") {
                    s = s.replace(/(\d{1,2})[\/\s](\d{1,2})[\/\s](\d{4})/, "$3/$1/$2");
                } else if (format === "ddmmyyyy") {
                    s = s.replace(/(\d{1,2})[\/\s](\d{1,2})[\/\s](\d{4})/, "$3/$2/$1");
                } else if (format === "yyyymmdd") {
                    s = s.replace(/(\d{4})[\/\s](\d{1,2})[\/\s](\d{1,2})/, "$1/$2/$3");
                }
            }
            return s ? ts.formatFloat( (new Date(s).getTime() || ''), table) : s;
        },
        type: "numeric"
    });

    ts.addParser({
        id: "time",
        is: function(s) {
            return (/^(([0-2]?\d:[0-5]\d)|([0-1]?\d:[0-5]\d\s?([AP]M)))$/i).test(s);
        },
        format: function(s, table) {
            return s ? ts.formatFloat( (new Date("2000/01/01 " + s.replace(/(\S)([AP]M)$/i, "$1 $2")).getTime() || ""), table) : s;
        },
        type: "numeric"
    });

    ts.addParser({
        id: "metadata",
        is: function() {
            return false;
        },
        format: function(s, table, cell) {
            var c = table.config,
                p = (!c.parserMetadataName) ? 'sortValue' : c.parserMetadataName;
            return $(cell).metadata()[p];
        },
        type: "numeric"
    });

    // add default widgets
    ts.addWidget({
        id: "zebra",
        priority: 90,
        format: function(table, c, wo) {
            var $tb, $tv, $tr, row, even, time, k, l,
                child = new RegExp(c.cssChildRow, 'i'),
                b = c.$tbodies;
            if (c.debug) {
                time = new Date();
            }
            for (k = 0; k < b.length; k++ ) {
                // loop through the visible rows
                $tb = b.eq(k);
                l = $tb.children('tr').length;
                if (l > 1) {
                    row = 0;
                    $tv = $tb.children('tr:visible');
                    // revered back to using jQuery each - strangely it's the fastest method
                    /*jshint loopfunc:true */
                    $tv.each(function(){
                        $tr = $(this);
                        // style children rows the same way the parent row was styled
                        if (!child.test(this.className)) { row++; }
                        even = (row % 2 === 0);
                        $tr.removeClass(wo.zebra[even ? 1 : 0]).addClass(wo.zebra[even ? 0 : 1]);
                    });
                }
            }
            if (c.debug) {
                ts.benchmark("Applying Zebra widget", time);
            }
        },
        remove: function(table, c, wo){
            var k, $tb,
                b = c.$tbodies,
                rmv = (wo.zebra || [ "even", "odd" ]).join(' ');
            for (k = 0; k < b.length; k++ ){
                $tb = $.tablesorter.processTbody(table, b.eq(k), true); // remove tbody
                $tb.children().removeClass(rmv);
                $.tablesorter.processTbody(table, $tb, false); // restore tbody
            }
        }
    });

})(jQuery);
/*->
 #name>Sortable Tables
 #javascript>Yes
 #css>Yes
 #description> Standards Patterns and Styling for HTML Sortable Tables
 <-*/
(function() {
    var DEFAULT_SORT_OPTIONS = {sortMultiSortKey: '', headers: {}, debug: false};

    function sortTable($table) {
        var options = DEFAULT_SORT_OPTIONS;
        $table.find("th").each(function(index, header) {

            var $header = AJS.$(header);
            options.headers[index] = {};
            if ($header.hasClass("aui-table-column-unsortable")) {
                options.headers[index].sorter = false;
            } else {
                $header.attr('tabindex', '0');
                $header.wrapInner("<span class='aui-table-header-content'/>");
                if ($header.hasClass("aui-table-column-issue-key")) {
                    options.headers[index].sorter = "issue-key";
                }
            }
        });
        $table.tablesorter(options);
    }

    AJS.tablessortable = {
        setup: function() {

            /*
             This parser is used for issue keys in the format <PROJECT_KEY>-<ISSUE_NUMBER>, where <PROJECT_KEY> is a maximum
             10 character string with characters(A-Z). Assumes that issue number is no larger than 999,999. e.g. not more
             than a million issues.
             This pads the issue key to allow for proper string sorting so that the project key is always 10 characters and the
             issue number is always 6 digits. e.g. it appends the project key '.' until it is 10 characters long and prepends 0
             so that the issue number is 6 digits long. e.g. CONF-102 == CONF......000102. This is to allow proper string sorting.
             */
            AJS.$.tablesorter.addParser({
                id: 'issue-key',
                is: function() {
                    return false;
                },

                format: function(s) {
                    var keyComponents = s.split("-");
                    var projectKey = keyComponents[0];
                    var issueNumber = keyComponents[1];

                    var PROJECT_KEY_TEMPLATE = "..........";
                    var ISSUE_NUMBER_TEMPLATE = "000000";
                    var stringRepresentation = (projectKey + PROJECT_KEY_TEMPLATE).slice(0, PROJECT_KEY_TEMPLATE.length);
                    stringRepresentation += (ISSUE_NUMBER_TEMPLATE + issueNumber).slice(-ISSUE_NUMBER_TEMPLATE.length);

                    return stringRepresentation;
                },

                type: 'text'
            });

            /*
             Text parser that uses the data-sort-value attribute for sorting if it is set and data-sort-type is not set
             or set to 'text'.
             */
            AJS.$.tablesorter.addParser({
                id: 'textSortAttributeParser',
                is: function (nodeValue, table, node) {
                    return node.hasAttribute('data-sort-value') && (!node.hasAttribute('data-sort-type') || node.getAttribute('data-sort-type') === 'text');
                },
                format: function (nodeValue, table, node, offset) {
                    return node.getAttribute('data-sort-value');
                },
                type: 'text'
            });

            /*
             Numeric parser that uses the data-sort-value attribute for sorting if it is set and data-sort-type is set
             to 'numeric'.
             */
            AJS.$.tablesorter.addParser({
                id: 'numericSortAttributeParser',
                is: function (nodeValue, table, node) {
                    return node.getAttribute('data-sort-type') === 'numeric' && node.hasAttribute('data-sort-value');
                },
                format: function (nodeValue, table, node, offset) {
                    return node.getAttribute('data-sort-value');
                },
                type: 'numeric'
            });

            AJS.$(".aui-table-sortable").each(function() {
                sortTable(AJS.$(this));
            });
        },

        setTableSortable: function($table) {
            sortTable($table);
        }
    };

    AJS.$(AJS.tablessortable.setup);
})();
(function ($) {
    'use strict';

    $.fn.tooltip = function (options) {

        // Pass string values straight to tipsy
        if (typeof options == 'string') {
            this.tipsy(options);
            return $this;
        }

        var allOptions = $.extend({}, $.fn.tooltip.defaults, options),
            $this = this.tipsy(allOptions);

        if (allOptions.hideOnClick &&
            (allOptions.trigger == 'hover' || !allOptions.trigger && this.tipsy.defaults.trigger == 'hover')) {
            var onClick = function() {
                $(this).tipsy('hide');
            };
            if (allOptions.live) {
                $(this.context).on('click.tipsy', this.selector, onClick);
            } else {
                this.bind('click.tipsy', onClick);
            }
        }
        return $this;
    };

    $.fn.tooltip.defaults = {
        opacity: 1.0,
        offset: 1,
        delayIn: 500,
        hoverable: true,
        hideOnClick: true
    };
}(AJS.$));

define('aui/form-validation/validator-register',[], function() {
    'use strict';

    var $ = AJS.$; // Can't depend on jQuery since it won't be defined in product environments (thanks to deletion of the define.amd object)

    var ATTRIBUTE_RESERVED_ARGUMENTS = ['displayfield', 'watchfield', 'when', 'novalidate', 'state'];
    var validators = [];

    function getReservedArgument (validatorArguments) {
        var reservedArgument = false;
        validatorArguments.some(function(arg) {
            var isReserved = $.inArray(arg, ATTRIBUTE_RESERVED_ARGUMENTS) !== -1;
            if (isReserved) {
                reservedArgument = arg;
            }
            return isReserved;
        });
        return reservedArgument;
    }

    /**
     * Register a validator that can be used to validate fields. The main entry point for validator plugins.
     * @param trigger - when to run the validator. Can be an array of arguments, or a selector
     * @param validatorFunction - the function that will be called on the field to determine validation. Receives
     *      field - the field that is being validated
     *      args - the arguments that have been specified in HTML markup.
     */
    function registerValidator (trigger, validatorFunction) {
        var triggerSelector;
        if (typeof trigger === 'string') {
            triggerSelector = trigger;
        } else {
            var reservedArgument = getReservedArgument(trigger);
            if (reservedArgument) {
                AJS.warn('Validators cannot be registered with the argument "' + reservedArgument + '", as it ' +
                    'is a reserved argument.');
                return false;
            }
            triggerSelector = '[data-aui-validation-' + trigger.join('],[data-aui-validation-') + ']';
        }

        var validator = {
            validatorFunction: validatorFunction,
            validatorTrigger: triggerSelector
        };

        validators.push(validator);

        return validator;
    }

    return {
        register: registerValidator,
        validators: function() {
            return validators;
        }
    };

});
;(function(init) {
    'use strict';

    define('aui/form-validation/basic-validators',['./validator-register'], function(validatorRegister) {
        return init(AJS, AJS.$, validatorRegister);
    });
})(function (AJS, $, validatorRegister) {
    'use strict';

    //Input length
    validatorRegister.register(['maxlength', 'minlength'], function(field) {
        var minlengthMessage = makeMessage('minlength', field.args);
        var maxlengthMessage = makeMessage('maxlength', field.args);

        if (field.$el.val().length < field.args('minlength')){
            field.invalidate(minlengthMessage);
        } else if (field.$el.val().length > field.args('maxlength')){
            field.invalidate(maxlengthMessage);
        } else {
            field.validate();
        }
    });

    //Field matching
    validatorRegister.register(['matchingfield'], function(field){
        var thisFieldValue = field.$el.val();
        var $matchingField = $('#' + field.args('matchingfield'));
        var matchingFieldValue = $matchingField.val();

        var matchingFieldMessage = makeMessage('matchingfield', field.args, [thisFieldValue, matchingFieldValue]);

        var shouldHidePasswords = isPasswordField(field.$el) || isPasswordField($matchingField);
        if (shouldHidePasswords) {
            matchingFieldMessage = makeMessage('matchingfield-novalue', field.args);
        }

        if (!thisFieldValue || !matchingFieldValue){
            field.validate();
        } else if (matchingFieldValue !== thisFieldValue) {
            field.invalidate(matchingFieldMessage);
        } else {
            field.validate();
        }
    });

    function isPasswordField($field) {
        return $field.attr('type') === 'password';
    }

    //Banned words
    validatorRegister.register(['doesnotcontain'], function(field) {
        var doesNotContainMessage = makeMessage('doesnotcontain', field.args);

        if(field.$el.val().indexOf(field.args('doesnotcontain')) === -1) {
            field.validate();
        } else {
            field.invalidate(doesNotContainMessage);
        }
    });

    //Matches regex
    validatorRegister.register(['pattern'], function(field) {
        var patternMessage = makeMessage('pattern', field.args);

        if(matchesRegex(field.$el.val(), new RegExp(field.args('pattern'), 'i'))) {
            field.validate();
        } else {
            field.invalidate(patternMessage);
        }
    });

    function matchesRegex(val, regex){
        var matches = val.match(regex);
        if (!matches) {
            return false;
        }
        var isExactMatch = (val === matches[0]);
        return isExactMatch;
    }

    //Required field
    validatorRegister.register(['required'], function(field) {
        var requiredMessage = makeMessage('required', field.args);

        if (field.$el.val()) {
            field.validate();
        } else {
            field.invalidate(requiredMessage);
        }
    });

    //Field value range (between min and max)
    validatorRegister.register(['min', 'max'], function(field) {
        var validNumberMessage = makeMessage('validnumber', field.args);
        var belowMinMessage = makeMessage('min', field.args);
        var aboveMaxMessage = makeMessage('max', field.args);

        var fieldValue = parseInt(field.$el.val());
        if (isNaN(fieldValue)) {
            field.invalidate(validNumberMessage);
            return;
        }
        if (field.args('min') && (fieldValue < parseInt(field.args('min')))) {
            field.invalidate(belowMinMessage);
        } else if(field.args('max') && (fieldValue > parseInt(field.args('max')))){
            field.invalidate(aboveMaxMessage);
        } else {
            field.validate();
        }
    });

    //Date format
    validatorRegister.register(['dateformat'], function(field) {
        var dateFormatSymbolic = field.args('dateformat');
        var dateFormatMessage = makeMessage('dateformat', field.args);

        var symbolRegexMap = {
            'Y': '[0-9]{4}',
            'y': '[0-9]{2}',
            'm': '(11|12|0{0,1}[0-9])',
            'M': '[Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec]',
            'D': '[Mon|Tue|Wed|Thu|Fri|Sat|Sun]',
            'd': '([0-2]{0,1}[0-9]{1})|(30|31)'
        };

        var dateFormatSymbolArray = dateFormatSymbolic.split('');
        var dateFormatRegexString = '';

        dateFormatSymbolArray.forEach(function(dateSymbol) {
            var isRecognisedSymbol = symbolRegexMap.hasOwnProperty(dateSymbol);
            if (isRecognisedSymbol) {
                dateFormatRegexString += symbolRegexMap[dateSymbol];
            } else {
                dateFormatRegexString += dateSymbol;
            }
        });

        var dateFormatRegex = new RegExp(dateFormatRegexString+'$', 'i');
        var isValidDate = matchesRegex(field.$el.val(), dateFormatRegex);

        if (isValidDate) {
            field.validate();
        } else {
            field.invalidate(dateFormatMessage);
        }
    });

    //Checkbox count
    validatorRegister.register(['minchecked', 'maxchecked'], function(field) {
        var amountChecked = field.$el.find(':checked').length;
        var aboveMin = !field.args('minchecked') || (amountChecked >= field.args('minchecked'));
        var belowMax = !field.args('maxchecked') || (amountChecked <= field.args('maxchecked'));

        var belowMinMessage = makeMessage('minchecked', field.args);
        var aboveMaxMessage = makeMessage('maxchecked', field.args);

        if (aboveMin && belowMax) {
            field.validate();
        } else if (!aboveMin) {
            field.invalidate(belowMinMessage);
        } else if (!belowMax) {
            field.invalidate(aboveMaxMessage);
        }
    });

    /*
         Retrieves a message for a plugin validator through the data attributes or the default (which is in the i18n file)
     */
    function makeMessage(key, accessorFunction, customTokens) {
        var inFlatpackMode = AJS.I18n.keys !== undefined;
        var defaultMessage;
        if (inFlatpackMode) {
            defaultMessage = AJS.I18n.keys['aui.validation.message.' + key];
        } else {
            defaultMessage = pluginI18nMessages[key];
        }

        var messageTokens = customTokens;
        if (!customTokens) {
            messageTokens = [accessorFunction(key)];
        }

        var customMessageUnformatted = accessorFunction(key+'-msg');
        var formattingArguments;

        if (customMessageUnformatted) {
            formattingArguments = [customMessageUnformatted].concat(messageTokens);
        } else {
            formattingArguments = [defaultMessage].concat(messageTokens);
        }

        return AJS.format.apply(null, formattingArguments);
    }

    /*
     The value AJS.I18n.getText('aui.validation.message...') (defaultMessage) cannot be refactored as it
     must appear verbatim for the plugin I18n transformation to pick it up
     */
    var pluginI18nMessages = {
        minlength: AJS.I18n.getText('aui.validation.message.minlength'),
        maxlength: AJS.I18n.getText('aui.validation.message.maxlength'),
        matchingfield: AJS.I18n.getText('aui.validation.message.matchingfield'),
        'matchingfield-novalue': AJS.I18n.getText('aui.validation.message.matchingfield-novalue'),
        doesnotcontain: AJS.I18n.getText('aui.validation.message.doesnotcontain'),
        pattern: AJS.I18n.getText('aui.validation.message.pattern'),
        required: AJS.I18n.getText('aui.validation.message.required'),
        validnumber: AJS.I18n.getText('aui.validation.message.validnumber'),
        min: AJS.I18n.getText('aui.validation.message.min'),
        max: AJS.I18n.getText('aui.validation.message.max'),
        dateformat: AJS.I18n.getText('aui.validation.message.dateformat'),
        minchecked: AJS.I18n.getText('aui.validation.message.minchecked'),
        maxchecked: AJS.I18n.getText('aui.validation.message.maxchecked')
    };

});
;(function(init) {
    'use strict';

    define('aui/form-notification',[],function () {
        return init(AJS.$, window.skate);
    });
})(function ($, skate) {
    'use strict';

    var NOTIFICATION_NAMESPACE = 'aui-form-notification';

    var ATTRIBUTE_TOOLTIP_POSITION = NOTIFICATION_NAMESPACE + '-position';

    var CLASS_NOTIFICATION_INITIALISED = '_aui-form-notification-initialised';

    var CLASS_NOTIFICATION_ICON = 'aui-icon-notification';

    var CLASS_TOOLTIP = NOTIFICATION_NAMESPACE + '-tooltip';
    var CLASS_TOOLTIP_ERROR = CLASS_TOOLTIP + '-error';
    var CLASS_TOOLTIP_INFO = CLASS_TOOLTIP + '-info';

    var ATTRIBUTE_NOTIFICATION_PREFIX = 'data-aui-notification-';
    var ATTRIBUTE_NOTIFICATION_WAIT = ATTRIBUTE_NOTIFICATION_PREFIX + 'wait';
    var ATTRIBUTE_NOTIFICATION_INFO = ATTRIBUTE_NOTIFICATION_PREFIX + 'info';
    var ATTRIBUTE_NOTIFICATION_ERROR = ATTRIBUTE_NOTIFICATION_PREFIX + 'error';
    var ATTRIBUTE_NOTIFICATION_SUCCESS = ATTRIBUTE_NOTIFICATION_PREFIX + 'success';

    var NOTIFICATION_PRIORITY = [
        ATTRIBUTE_NOTIFICATION_ERROR,
        ATTRIBUTE_NOTIFICATION_SUCCESS,
        ATTRIBUTE_NOTIFICATION_WAIT,
        ATTRIBUTE_NOTIFICATION_INFO
    ];

    var notificationFields = [];

    /* --- Tipsy configuration --- */
    var TIPSY_OPACITY = 1;
    var TIPSY_OFFSET_INSIDE_FIELD = 9; //offset in px from the icon to the start of the tipsy
    var TIPSY_OFFSET_OUTSIDE_FIELD = 3;

    function initialiseNotification($field) {
        if (!isFieldInitialised($field)) {
            prepareFieldMarkup($field);
            initialiseTooltip($field);
            bindFieldEvents($field);
        }

        notificationFields.push($field);
    }

    function isFieldInitialised($field) {
        return $field.hasClass(CLASS_NOTIFICATION_INITIALISED);
    }

    function constructFieldIcon(){
        return $('<span class="aui-icon aui-icon-small ' + CLASS_NOTIFICATION_ICON + '"/>');
    }

    function prepareFieldMarkup($field) {
        $field.addClass(CLASS_NOTIFICATION_INITIALISED);
        appendIconToField($field);
    }

    function appendIconToField($field) {
        var $icon = constructFieldIcon();
        $field.after($icon);
    }

    function initialiseTooltip($field) {
        getTooltipAnchor($field).tipsy({
            gravity: getTipsyGravity($field),
            title: function(){
                return getNotificationMessage($field);
            },
            trigger: 'manual',
            offset: canContainIcon($field) ? TIPSY_OFFSET_INSIDE_FIELD : TIPSY_OFFSET_OUTSIDE_FIELD,
            opacity: TIPSY_OPACITY,
            className: function() {
                return 'aui-form-notification-tooltip ' + getNotificationClass($field);
            },
            html: true
        });
    }

    // A list of HTML5 input types that don't typically get augmented by the browser, so are safe to put icons inside of.
    var unadornedInputFields = ['text', 'url', 'email', 'tel', 'password'];

    function canContainIcon($field) {
        return unadornedInputFields.indexOf($field.attr('type')) !== -1;
    }

    function getNotificationMessage($field) {
        var notificationType = getFieldNotificationType($field);
        var message = notificationType ? $field.attr(notificationType) : '';
        return formatMessage(message);
    }

    function formatMessage(message) {
        if (message === '') {
            return message;
        }

        var messageArray = jsonToArray(message);

        if (messageArray.length === 1) {
            return messageArray[0];
        } else {
            return '<ul><li>'+messageArray.join('</li><li>')+'</li></ul>';
        }
    }

    function jsonToArray(jsonOrString) {
        var jsonArray;
        try {
            jsonArray = JSON.parse(jsonOrString);
        } catch (exception) {
            jsonArray = [jsonOrString];
        }
        return jsonArray;
    }

    function getNotificationClass($field) {
        var notificationType = getFieldNotificationType($field);

        if (notificationType === ATTRIBUTE_NOTIFICATION_ERROR) {
            return CLASS_TOOLTIP_ERROR;
        } else if (notificationType === ATTRIBUTE_NOTIFICATION_INFO) {
            return CLASS_TOOLTIP_INFO;
        }
    }

    function getFieldNotificationType($field) {
        var fieldNotificationType;
        NOTIFICATION_PRIORITY.some(function(prioritisedNotification) {
            if ($field.is('[' + prioritisedNotification + ']')) {
                fieldNotificationType = prioritisedNotification;
                return true;
            }
        });

        return fieldNotificationType;
    }

    function bindFieldEvents($field) {
        if (focusTogglesTooltip($field)) {
            bindFieldTabEvents($field);
        }
    }

    function focusTogglesTooltip($field) {
        return $field.is(':aui-focusable');
    }

    function fieldHasTooltip($field) {
        return getNotificationMessage($field) !== '';
    }

    function showTooltip($field) {
        getTooltipAnchor($field).tipsy('show');
        if (focusTogglesTooltip($field)) {
            bindTooltipTabEvents($field);
        }
    }

    function hideTooltip($field) {
        getTooltipAnchor($field).tipsy('hide');
    }

    function bindFocusTooltipInteractions() {
        document.addEventListener('focus', function(e) {
            notificationFields.forEach(function(field) {
                var $field = $(field);
                var $tooltip = getTooltip($field);

                if (!$tooltip || !focusTogglesTooltip($field)) {
                    return;
                }

                var isFocusInTooltip = $.contains($tooltip[0], e.target);
                var isFocusTargetField = $field.is(e.target);

                if (isFocusTargetField) {
                    showTooltip($field);
                } else if (!isFocusInTooltip) {
                    hideTooltip($field);
                }
            });
        }, true);
    }

    bindFocusTooltipInteractions();

    function bindFieldTabEvents($field) {
        $field.on('keydown', function(e) {
            if (isNormalTab(e) && fieldHasTooltip($field)) {
                var $firstTooltipLink = getFirstTooltipLink($field);
                if ($firstTooltipLink.length) {
                    $firstTooltipLink.focus();
                    e.preventDefault();
                }
            }
        });
    }

    function isNormalTab(e) {
        return e.keyCode === AJS.keyCode.TAB && !e.shiftKey && !e.altKey;
    }

    function isShiftTab(e) {
        return e.keyCode === AJS.keyCode.TAB && e.shiftKey;
    }

    function getFirstTooltipLink($field) {
        return getTooltip($field).find(':aui-tabbable').first();
    }

    function getLastTooltipLink($field) {
        return getTooltip($field).find(':aui-tabbable').last();
    }

    function getTooltip($field) {
        var $anchor = getTooltipAnchor($field);
        if ($anchor.data('tipsy')) {
            return $anchor.data('tipsy').$tip;
        }
    }

    function bindTooltipTabEvents($field) {
        var $tooltip = getTooltip($field);
        $tooltip.on('keydown', function(e) {
            var leavingTooltipForwards = elementIsActive(getLastTooltipLink($field));
            var leavingTooltipBackwards = elementIsActive(getFirstTooltipLink($field));

            if (isNormalTab(e) && leavingTooltipForwards) {
                if (leavingTooltipForwards) {
                    $field.focus();
                }
            }
            if (isShiftTab(e) && leavingTooltipBackwards) {
                if (leavingTooltipBackwards) {
                    $field.focus();
                    e.preventDefault();
                }
            }
        });
    }

    function getTipsyGravity($field) {
        var position = $field.data(ATTRIBUTE_TOOLTIP_POSITION) || 'side';
        var gravityMap = {
            side: 'w',
            top: 'se',
            bottom: 'ne'
        };
        var gravity = gravityMap[position];
        if (!gravity) {
            gravity = 'w';
            AJS.warn('Invalid notification position: "' + position + '". Valid options are "side", "bottom, "top"');
        }
        return gravity;
    }

    function getTooltipAnchor($field) {
        return getFieldIcon($field);
    }

    function getFieldIcon($field) {
        return $field.next('.' + CLASS_NOTIFICATION_ICON);
    }

    function elementIsActive($el) {
        var el = ($el instanceof $) ? $el[0]: $el;
        return el && el === document.activeElement;
    }

    function synchroniseNotificationDisplay($field) {
        $field = ($field instanceof $) ? $field : $($field);
        var notificationType = getFieldNotificationType($field);

        var showSpinner = notificationType === ATTRIBUTE_NOTIFICATION_WAIT;
        setFieldSpinner($field, showSpinner);
        var noNotificationOnField = !notificationType;
        if (noNotificationOnField) {
            hideTooltip($field);
            return;
        }

        var message = getNotificationMessage($field);

        var tooltipShouldBeVisible = (elementIsActive($field) || !canContainIcon($field));
        if (tooltipShouldBeVisible && message) {
            showTooltip($field);
        } else {
            hideTooltip($field);
        }
    }

    function setFieldSpinner($field, isSpinnerVisible) {
        if (isSpinnerVisible) {
            getFieldIcon($field).addClass('aui-icon-wait');
        } else {
            getFieldIcon($field).removeClass('aui-icon-wait');
        }
    }

    document.addEventListener('mousedown', function(e) {
        var isTargetNotificationField = $(e.target).attr('data-aui-notification-field') !== undefined;
        if (isTargetNotificationField) {
            return;
        }

        var isTargetLink = $(e.target).is('a');
        if (isTargetLink) {
            return;
        }

        var isTargetTooltip = $(e.target).closest('.aui-form-notification-tooltip').length > 0;
        if (isTargetTooltip) {
            return;
        }

        var $allNotificationFields = $('[data-aui-notification-field]');
        $allNotificationFields.each(function() {
            var $notificationField = $(this);
            hideTooltip($notificationField);
        });
    });

    skate('data-aui-notification-field', {
        insert: function(element) {
            initialiseNotification($(element));
        },
        attributes: (function() {
            var attrs = {};
            NOTIFICATION_PRIORITY.forEach(function(type) {
                attrs[type] = synchroniseNotificationDisplay;
            });
            return attrs;
        }()),
        type: skate.types.ATTR
    });
});

;(function(init) {
    'use strict';

    define('aui/form-validation',['./form-validation/validator-register', './form-validation/basic-validators', './form-notification'], function(validatorRegister) {
        return init(AJS.$, window.skate, validatorRegister);
    });
})(function ($, skate, validatorRegister) {
    'use strict';

    //Attributes
    var ATTRIBUTE_VALIDATION_OPTION_PREFIX = 'aui-validation-';
    var ATTRIBUTE_NOTIFICATION_PREFIX = 'data-aui-notification-';

    var ATTRIBUTE_FIELD_STATE = 'aui-validation-state';
    var INVALID = 'invalid';
    var VALID = 'valid';
    var VALIDATING = 'validating';
    var UNVALIDATED = 'unvalidated';

    var ATTRIBUTE_VALIDATION_FIELD_COMPONENT = 'data-aui-validation-field';

    //Classes
    var CLASS_VALIDATION_INITIALISED = '_aui-form-validation-initialised';

    //Events
    var EVENT_FIELD_STATE_CHANGED = '_aui-internal-field-state-changed';

    function isFieldInitialised($field) {
        return $field.hasClass(CLASS_VALIDATION_INITIALISED);
    }

    function initValidation($field) {
        if (!isFieldInitialised($field)) {
            initialiseDisplayField($field);
            prepareFieldMarkup($field);
            bindFieldEvents($field);
            changeFieldState($field, UNVALIDATED);
        }
    }

    function initialiseDisplayField($field) {
        getDisplayField($field).attr('data-aui-notification-field', '');
    }

    function prepareFieldMarkup($field){
        $field.addClass(CLASS_VALIDATION_INITIALISED);
    }

    function bindFieldEvents($field) {
        bindStopTypingEvent($field);
        bindValidationEvent($field);
    }

    function bindStopTypingEvent($field){
        var keyUpTimer;

        var triggerStopTypingEvent = function(){
            $field.trigger('aui-stop-typing');
        };

        $field.on('keyup', function(){
            clearTimeout(keyUpTimer);
            keyUpTimer = setTimeout(triggerStopTypingEvent, 1500);
        });
    }

    function bindValidationEvent($field) {
        var validateWhen = getValidationOption($field, 'when');
        var watchedFieldID = getValidationOption($field, 'watchfield');

        var watchElements = watchedFieldID ? $field.add('#' + watchedFieldID) : $field;

        watchElements.on(validateWhen, makeStartValidationHandler($field));
    }

    function makeStartValidationHandler($field) {
        return function() {
            var noValidate = getValidationOption($field, 'novalidate');

            if (noValidate) {
                return;
            }

            startValidating($field);
        };
    }

    function getValidationOption($field, option) {
        var defaults = {
            'when': 'change'
        };
        var optionValue = $field.data(ATTRIBUTE_VALIDATION_OPTION_PREFIX + option);
        if (!optionValue) {
            optionValue = defaults[option];
        }

        return optionValue;
    }

    function startValidating($field) {
        clearFieldMessages($field);

        var validatorsToRun = getActivatedValidators($field);

        changeFieldState($field, VALIDATING);

        var deferreds = runValidatorsAndGetDeferred($field, validatorsToRun);
        $.when.apply($, deferreds).done(function(){
            changeFieldState($field, VALID);
        });
    }

    function clearFieldMessages($field) {
        setFieldNotification(getDisplayField($field), 'none');
    }

    function getValidators() {
        return validatorRegister.validators();
    }

    function getActivatedValidators($field) {
        var callList = [];
        getValidators().forEach(function(validator, index) {
            var validatorTrigger = validator.validatorTrigger;
            var runThisValidator = $field.is(validatorTrigger);
            if (runThisValidator) {
                callList.push(index);
            }
        });

        return callList;
    }

    function runValidatorsAndGetDeferred($field, validatorsToRun) {
        var allDeferreds = [];

        validatorsToRun.forEach(function(validatorIndex) {
            var validatorFunction = getValidators()[validatorIndex].validatorFunction;
            var deferred = new $.Deferred();
            var validatorContext = createValidatorContext($field, deferred);
            validatorFunction(validatorContext);

            allDeferreds.push(deferred);
        });

        return allDeferreds;
    }

    function createValidatorContext($field, validatorDeferred) {
        return {
            validate: function(){
                validatorDeferred.resolve();
            },
            invalidate: function(message){
                changeFieldState($field, INVALID, message);
                validatorDeferred.reject();
            },
            args: createArgumentAccessorFunction($field),
            $el: $field
        };
    }

    function createArgumentAccessorFunction($field) {
        return function(arg) {
            return $field.data(ATTRIBUTE_VALIDATION_OPTION_PREFIX + arg);
        };
    }

    function changeFieldState($field, state, message) {
        $field.attr('data-'+ATTRIBUTE_FIELD_STATE, state);

        if (state === UNVALIDATED) {
            return;
        }

        $field.trigger($.Event(EVENT_FIELD_STATE_CHANGED));

        var $displayField = getDisplayField($field);

        var stateToNotificationTypeMap = {};
        stateToNotificationTypeMap[VALIDATING] = 'wait';
        stateToNotificationTypeMap[INVALID] = 'error';
        stateToNotificationTypeMap[VALID] = 'success';

        var notificationType = stateToNotificationTypeMap[state];

        if (state === VALIDATING) {
            showSpinnerIfSlow($field);
        } else {
            setFieldNotification($displayField, notificationType, message);
        }

    }

    function showSpinnerIfSlow($field) {
        setTimeout(function() {
            var stillValidating = getFieldState($field) === VALIDATING;
            if (stillValidating) {
                setFieldNotification($field, 'wait');
            }
        }, 500);
    }

    function setFieldNotification($field, type, message) {
        var spinnerWasVisible = isSpinnerVisible($field);
        removeIconOnlyNotifications($field);
        var skipShowingSuccessNotification = (type === 'success') && !spinnerWasVisible;
        if (skipShowingSuccessNotification) {
            return;
        }

        if (type === 'none') {
            removeFieldNotification($field, 'error');
        } else {
            var previousMessage = $field.attr(ATTRIBUTE_NOTIFICATION_PREFIX + type) || '[]';
            var newMessage = message ? combineJSONMessages(message, previousMessage) : '';
            $field.attr(ATTRIBUTE_NOTIFICATION_PREFIX + type, newMessage);
        }
    }

    function removeIconOnlyNotifications($field) {
        removeFieldNotification($field, 'wait');
        removeFieldNotification($field, 'success');
    }

    function removeFieldNotification($field, type) {
        $field.removeAttr(ATTRIBUTE_NOTIFICATION_PREFIX + type);
    }

    function isSpinnerVisible($field) {
        return $field.is('[' + ATTRIBUTE_NOTIFICATION_PREFIX + 'wait]');
    }

    function combineJSONMessages(newString, previousString) {
        var previousStackedMessageList = JSON.parse(previousString);
        var newStackedMessageList = previousStackedMessageList.concat([newString]);
        var newStackedMessage = JSON.stringify(newStackedMessageList);
        return newStackedMessage;
    }

    function getDisplayField($field){
        var displayFieldID = getValidationOption($field, 'displayfield');
        var notifyOnSelf = (displayFieldID === undefined);
        return notifyOnSelf ? $field : $('#'+displayFieldID);
    }

    function getFieldState($field) {
        return $field.attr('data-'+ATTRIBUTE_FIELD_STATE);
    }

    /**
     * Trigger validation on a field manually
     * @param $field the field that validation should be triggered for
     */
    function validateField($field) {
        makeStartValidationHandler($field)();
    }

    /**
     * Form scrolling and submission prevent based on validation state
     * -If the form is unvalidated, validate all fields
     * -If the form is invalid, go to the first invalid element
     * -If the form is validating, wait for them to validate and then try submitting again
     * -If the form is valid, allow form submission
     */
    $(document).on('submit', function(e) {
        var form = e.target;
        var $form = $(form);

        var formState = getFormStateName($form);
        if (formState === UNVALIDATED) {
            validateUnvalidatedFields($form);
            delayFormSubmission($form, e);
        } else if (formState === VALIDATING) {
            delayFormSubmission($form, e);
        } else if (formState === INVALID) {
            e.preventDefault();
            selectFirstInvalid($form);
        } else if (formState === VALID) {
            var validSubmitEvent = $.Event('aui-valid-submit');
            $form.trigger(validSubmitEvent);
            var preventNormalSubmit = validSubmitEvent.isDefaultPrevented();
            if (preventNormalSubmit) {
                e.preventDefault(); //users can bind to aui-valid-submit for ajax forms
            }
        }
    });

    function delayFormSubmission($form, event) {
        event.preventDefault();
        var isFormValidating = getFormStateName($form) === VALIDATING;
        if (!isFormValidating) {
            $form.trigger('submit');
        } else {
            $form.one(EVENT_FIELD_STATE_CHANGED, function() {
                $form.trigger('submit');
            });
        }
    }

    function getFormStateName($form) {
        var $fieldCollection = $form.find('.' + CLASS_VALIDATION_INITIALISED);
        var fieldStates = getFieldCollectionStateNames($fieldCollection);
        var wholeFormState = mergeStates(fieldStates);
        return wholeFormState;
    }

    function getFieldCollectionStateNames($fields) {
        var states = $.map($fields, function(field) {
            return getFieldState($(field));
        });
        return states;
    }

    function mergeStates(stateNames) {
        var containsInvalidState = stateNames.indexOf(INVALID) !== -1;
        var containsUnvalidatedState = stateNames.indexOf(UNVALIDATED) !== -1;
        var containsValidatingState = stateNames.indexOf(VALIDATING) !== -1;
        var containsValidState = stateNames.indexOf(VALID) !== -1;

        if (containsInvalidState) {
            return INVALID;
        } else if (containsUnvalidatedState) {
            return UNVALIDATED;
        } else if (containsValidatingState) {
            return VALIDATING;
        } else if (containsValidState) {
            return VALID;
        }
    }

    function validateUnvalidatedFields($form) {
        var $unvalidatedElements = getFieldsInFormWithState($form, UNVALIDATED);
        $unvalidatedElements.each(function(index, el) {
            validator.validate($(el));
        });
    }

    function selectFirstInvalid($form) {
        var $firstInvalidField = getFieldsInFormWithState($form, INVALID).first();
        $firstInvalidField.focus();
    }

    function getFieldsInFormWithState($form, state) {
        var selector = '[data-'+ATTRIBUTE_FIELD_STATE+'='+state+']';
        return $form.find(selector);
    }


    var validator = {
        register: validatorRegister.register,
        validate: validateField
    };

    skate(ATTRIBUTE_VALIDATION_FIELD_COMPONENT, {
        insert: function(field) {
            var $field = $(field);
            initValidation($field);
            skate.init(field); //needed to kick off form notification skate initialisation
        },
        type: skate.types.ATTR
    });


    return validator;
});

/*
 Copyright 2012 Igor Vaynberg

 Version: 3.4.5 Timestamp: Mon Nov  4 08:22:42 PST 2013

 This software is licensed under the Apache License, Version 2.0 (the "Apache License") or the GNU
 General Public License version 2 (the "GPL License"). You may choose either license to govern your
 use of this software only upon the condition that you accept all of the terms of either the Apache
 License or the GPL License.

 You may obtain a copy of the Apache License and the GPL License at:

 http://www.apache.org/licenses/LICENSE-2.0
 http://www.gnu.org/licenses/gpl-2.0.html

 Unless required by applicable law or agreed to in writing, software distributed under the
 Apache License or the GPL Licesnse is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 CONDITIONS OF ANY KIND, either express or implied. See the Apache License and the GPL License for
 the specific language governing permissions and limitations under the Apache License and the GPL License.
 */
(function ($) {
    if(typeof $.fn.each2 == "undefined") {
        $.extend($.fn, {
            /*
             * 4-10 times faster .each replacement
             * use it carefully, as it overrides jQuery context of element on each iteration
             */
            each2 : function (c) {
                var j = $([0]), i = -1, l = this.length;
                while (
                    ++i < l
                        && (j.context = j[0] = this[i])
                        && c.call(j[0], i, j) !== false //"this"=DOM, i=index, j=jQuery object
                    );
                return this;
            }
        });
    }
})(jQuery);

(function ($, undefined) {
    "use strict";
    /*global document, window, jQuery, console */

    if (window.Select2 !== undefined) {
        return;
    }

    var KEY, AbstractSelect2, SingleSelect2, MultiSelect2, nextUid, sizer,
        lastMousePosition={x:0,y:0}, $document, scrollBarDimensions,

        KEY = {
            TAB: 9,
            ENTER: 13,
            ESC: 27,
            SPACE: 32,
            LEFT: 37,
            UP: 38,
            RIGHT: 39,
            DOWN: 40,
            SHIFT: 16,
            CTRL: 17,
            ALT: 18,
            PAGE_UP: 33,
            PAGE_DOWN: 34,
            HOME: 36,
            END: 35,
            BACKSPACE: 8,
            DELETE: 46,
            isArrow: function (k) {
                k = k.which ? k.which : k;
                switch (k) {
                    case KEY.LEFT:
                    case KEY.RIGHT:
                    case KEY.UP:
                    case KEY.DOWN:
                        return true;
                }
                return false;
            },
            isControl: function (e) {
                var k = e.which;
                switch (k) {
                    case KEY.SHIFT:
                    case KEY.CTRL:
                    case KEY.ALT:
                        return true;
                }

                if (e.metaKey) return true;

                return false;
            },
            isFunctionKey: function (k) {
                k = k.which ? k.which : k;
                return k >= 112 && k <= 123;
            }
        },
        MEASURE_SCROLLBAR_TEMPLATE = "<div class='select2-measure-scrollbar'></div>",

        DIACRITICS = {"\u24B6":"A","\uFF21":"A","\u00C0":"A","\u00C1":"A","\u00C2":"A","\u1EA6":"A","\u1EA4":"A","\u1EAA":"A","\u1EA8":"A","\u00C3":"A","\u0100":"A","\u0102":"A","\u1EB0":"A","\u1EAE":"A","\u1EB4":"A","\u1EB2":"A","\u0226":"A","\u01E0":"A","\u00C4":"A","\u01DE":"A","\u1EA2":"A","\u00C5":"A","\u01FA":"A","\u01CD":"A","\u0200":"A","\u0202":"A","\u1EA0":"A","\u1EAC":"A","\u1EB6":"A","\u1E00":"A","\u0104":"A","\u023A":"A","\u2C6F":"A","\uA732":"AA","\u00C6":"AE","\u01FC":"AE","\u01E2":"AE","\uA734":"AO","\uA736":"AU","\uA738":"AV","\uA73A":"AV","\uA73C":"AY","\u24B7":"B","\uFF22":"B","\u1E02":"B","\u1E04":"B","\u1E06":"B","\u0243":"B","\u0182":"B","\u0181":"B","\u24B8":"C","\uFF23":"C","\u0106":"C","\u0108":"C","\u010A":"C","\u010C":"C","\u00C7":"C","\u1E08":"C","\u0187":"C","\u023B":"C","\uA73E":"C","\u24B9":"D","\uFF24":"D","\u1E0A":"D","\u010E":"D","\u1E0C":"D","\u1E10":"D","\u1E12":"D","\u1E0E":"D","\u0110":"D","\u018B":"D","\u018A":"D","\u0189":"D","\uA779":"D","\u01F1":"DZ","\u01C4":"DZ","\u01F2":"Dz","\u01C5":"Dz","\u24BA":"E","\uFF25":"E","\u00C8":"E","\u00C9":"E","\u00CA":"E","\u1EC0":"E","\u1EBE":"E","\u1EC4":"E","\u1EC2":"E","\u1EBC":"E","\u0112":"E","\u1E14":"E","\u1E16":"E","\u0114":"E","\u0116":"E","\u00CB":"E","\u1EBA":"E","\u011A":"E","\u0204":"E","\u0206":"E","\u1EB8":"E","\u1EC6":"E","\u0228":"E","\u1E1C":"E","\u0118":"E","\u1E18":"E","\u1E1A":"E","\u0190":"E","\u018E":"E","\u24BB":"F","\uFF26":"F","\u1E1E":"F","\u0191":"F","\uA77B":"F","\u24BC":"G","\uFF27":"G","\u01F4":"G","\u011C":"G","\u1E20":"G","\u011E":"G","\u0120":"G","\u01E6":"G","\u0122":"G","\u01E4":"G","\u0193":"G","\uA7A0":"G","\uA77D":"G","\uA77E":"G","\u24BD":"H","\uFF28":"H","\u0124":"H","\u1E22":"H","\u1E26":"H","\u021E":"H","\u1E24":"H","\u1E28":"H","\u1E2A":"H","\u0126":"H","\u2C67":"H","\u2C75":"H","\uA78D":"H","\u24BE":"I","\uFF29":"I","\u00CC":"I","\u00CD":"I","\u00CE":"I","\u0128":"I","\u012A":"I","\u012C":"I","\u0130":"I","\u00CF":"I","\u1E2E":"I","\u1EC8":"I","\u01CF":"I","\u0208":"I","\u020A":"I","\u1ECA":"I","\u012E":"I","\u1E2C":"I","\u0197":"I","\u24BF":"J","\uFF2A":"J","\u0134":"J","\u0248":"J","\u24C0":"K","\uFF2B":"K","\u1E30":"K","\u01E8":"K","\u1E32":"K","\u0136":"K","\u1E34":"K","\u0198":"K","\u2C69":"K","\uA740":"K","\uA742":"K","\uA744":"K","\uA7A2":"K","\u24C1":"L","\uFF2C":"L","\u013F":"L","\u0139":"L","\u013D":"L","\u1E36":"L","\u1E38":"L","\u013B":"L","\u1E3C":"L","\u1E3A":"L","\u0141":"L","\u023D":"L","\u2C62":"L","\u2C60":"L","\uA748":"L","\uA746":"L","\uA780":"L","\u01C7":"LJ","\u01C8":"Lj","\u24C2":"M","\uFF2D":"M","\u1E3E":"M","\u1E40":"M","\u1E42":"M","\u2C6E":"M","\u019C":"M","\u24C3":"N","\uFF2E":"N","\u01F8":"N","\u0143":"N","\u00D1":"N","\u1E44":"N","\u0147":"N","\u1E46":"N","\u0145":"N","\u1E4A":"N","\u1E48":"N","\u0220":"N","\u019D":"N","\uA790":"N","\uA7A4":"N","\u01CA":"NJ","\u01CB":"Nj","\u24C4":"O","\uFF2F":"O","\u00D2":"O","\u00D3":"O","\u00D4":"O","\u1ED2":"O","\u1ED0":"O","\u1ED6":"O","\u1ED4":"O","\u00D5":"O","\u1E4C":"O","\u022C":"O","\u1E4E":"O","\u014C":"O","\u1E50":"O","\u1E52":"O","\u014E":"O","\u022E":"O","\u0230":"O","\u00D6":"O","\u022A":"O","\u1ECE":"O","\u0150":"O","\u01D1":"O","\u020C":"O","\u020E":"O","\u01A0":"O","\u1EDC":"O","\u1EDA":"O","\u1EE0":"O","\u1EDE":"O","\u1EE2":"O","\u1ECC":"O","\u1ED8":"O","\u01EA":"O","\u01EC":"O","\u00D8":"O","\u01FE":"O","\u0186":"O","\u019F":"O","\uA74A":"O","\uA74C":"O","\u01A2":"OI","\uA74E":"OO","\u0222":"OU","\u24C5":"P","\uFF30":"P","\u1E54":"P","\u1E56":"P","\u01A4":"P","\u2C63":"P","\uA750":"P","\uA752":"P","\uA754":"P","\u24C6":"Q","\uFF31":"Q","\uA756":"Q","\uA758":"Q","\u024A":"Q","\u24C7":"R","\uFF32":"R","\u0154":"R","\u1E58":"R","\u0158":"R","\u0210":"R","\u0212":"R","\u1E5A":"R","\u1E5C":"R","\u0156":"R","\u1E5E":"R","\u024C":"R","\u2C64":"R","\uA75A":"R","\uA7A6":"R","\uA782":"R","\u24C8":"S","\uFF33":"S","\u1E9E":"S","\u015A":"S","\u1E64":"S","\u015C":"S","\u1E60":"S","\u0160":"S","\u1E66":"S","\u1E62":"S","\u1E68":"S","\u0218":"S","\u015E":"S","\u2C7E":"S","\uA7A8":"S","\uA784":"S","\u24C9":"T","\uFF34":"T","\u1E6A":"T","\u0164":"T","\u1E6C":"T","\u021A":"T","\u0162":"T","\u1E70":"T","\u1E6E":"T","\u0166":"T","\u01AC":"T","\u01AE":"T","\u023E":"T","\uA786":"T","\uA728":"TZ","\u24CA":"U","\uFF35":"U","\u00D9":"U","\u00DA":"U","\u00DB":"U","\u0168":"U","\u1E78":"U","\u016A":"U","\u1E7A":"U","\u016C":"U","\u00DC":"U","\u01DB":"U","\u01D7":"U","\u01D5":"U","\u01D9":"U","\u1EE6":"U","\u016E":"U","\u0170":"U","\u01D3":"U","\u0214":"U","\u0216":"U","\u01AF":"U","\u1EEA":"U","\u1EE8":"U","\u1EEE":"U","\u1EEC":"U","\u1EF0":"U","\u1EE4":"U","\u1E72":"U","\u0172":"U","\u1E76":"U","\u1E74":"U","\u0244":"U","\u24CB":"V","\uFF36":"V","\u1E7C":"V","\u1E7E":"V","\u01B2":"V","\uA75E":"V","\u0245":"V","\uA760":"VY","\u24CC":"W","\uFF37":"W","\u1E80":"W","\u1E82":"W","\u0174":"W","\u1E86":"W","\u1E84":"W","\u1E88":"W","\u2C72":"W","\u24CD":"X","\uFF38":"X","\u1E8A":"X","\u1E8C":"X","\u24CE":"Y","\uFF39":"Y","\u1EF2":"Y","\u00DD":"Y","\u0176":"Y","\u1EF8":"Y","\u0232":"Y","\u1E8E":"Y","\u0178":"Y","\u1EF6":"Y","\u1EF4":"Y","\u01B3":"Y","\u024E":"Y","\u1EFE":"Y","\u24CF":"Z","\uFF3A":"Z","\u0179":"Z","\u1E90":"Z","\u017B":"Z","\u017D":"Z","\u1E92":"Z","\u1E94":"Z","\u01B5":"Z","\u0224":"Z","\u2C7F":"Z","\u2C6B":"Z","\uA762":"Z","\u24D0":"a","\uFF41":"a","\u1E9A":"a","\u00E0":"a","\u00E1":"a","\u00E2":"a","\u1EA7":"a","\u1EA5":"a","\u1EAB":"a","\u1EA9":"a","\u00E3":"a","\u0101":"a","\u0103":"a","\u1EB1":"a","\u1EAF":"a","\u1EB5":"a","\u1EB3":"a","\u0227":"a","\u01E1":"a","\u00E4":"a","\u01DF":"a","\u1EA3":"a","\u00E5":"a","\u01FB":"a","\u01CE":"a","\u0201":"a","\u0203":"a","\u1EA1":"a","\u1EAD":"a","\u1EB7":"a","\u1E01":"a","\u0105":"a","\u2C65":"a","\u0250":"a","\uA733":"aa","\u00E6":"ae","\u01FD":"ae","\u01E3":"ae","\uA735":"ao","\uA737":"au","\uA739":"av","\uA73B":"av","\uA73D":"ay","\u24D1":"b","\uFF42":"b","\u1E03":"b","\u1E05":"b","\u1E07":"b","\u0180":"b","\u0183":"b","\u0253":"b","\u24D2":"c","\uFF43":"c","\u0107":"c","\u0109":"c","\u010B":"c","\u010D":"c","\u00E7":"c","\u1E09":"c","\u0188":"c","\u023C":"c","\uA73F":"c","\u2184":"c","\u24D3":"d","\uFF44":"d","\u1E0B":"d","\u010F":"d","\u1E0D":"d","\u1E11":"d","\u1E13":"d","\u1E0F":"d","\u0111":"d","\u018C":"d","\u0256":"d","\u0257":"d","\uA77A":"d","\u01F3":"dz","\u01C6":"dz","\u24D4":"e","\uFF45":"e","\u00E8":"e","\u00E9":"e","\u00EA":"e","\u1EC1":"e","\u1EBF":"e","\u1EC5":"e","\u1EC3":"e","\u1EBD":"e","\u0113":"e","\u1E15":"e","\u1E17":"e","\u0115":"e","\u0117":"e","\u00EB":"e","\u1EBB":"e","\u011B":"e","\u0205":"e","\u0207":"e","\u1EB9":"e","\u1EC7":"e","\u0229":"e","\u1E1D":"e","\u0119":"e","\u1E19":"e","\u1E1B":"e","\u0247":"e","\u025B":"e","\u01DD":"e","\u24D5":"f","\uFF46":"f","\u1E1F":"f","\u0192":"f","\uA77C":"f","\u24D6":"g","\uFF47":"g","\u01F5":"g","\u011D":"g","\u1E21":"g","\u011F":"g","\u0121":"g","\u01E7":"g","\u0123":"g","\u01E5":"g","\u0260":"g","\uA7A1":"g","\u1D79":"g","\uA77F":"g","\u24D7":"h","\uFF48":"h","\u0125":"h","\u1E23":"h","\u1E27":"h","\u021F":"h","\u1E25":"h","\u1E29":"h","\u1E2B":"h","\u1E96":"h","\u0127":"h","\u2C68":"h","\u2C76":"h","\u0265":"h","\u0195":"hv","\u24D8":"i","\uFF49":"i","\u00EC":"i","\u00ED":"i","\u00EE":"i","\u0129":"i","\u012B":"i","\u012D":"i","\u00EF":"i","\u1E2F":"i","\u1EC9":"i","\u01D0":"i","\u0209":"i","\u020B":"i","\u1ECB":"i","\u012F":"i","\u1E2D":"i","\u0268":"i","\u0131":"i","\u24D9":"j","\uFF4A":"j","\u0135":"j","\u01F0":"j","\u0249":"j","\u24DA":"k","\uFF4B":"k","\u1E31":"k","\u01E9":"k","\u1E33":"k","\u0137":"k","\u1E35":"k","\u0199":"k","\u2C6A":"k","\uA741":"k","\uA743":"k","\uA745":"k","\uA7A3":"k","\u24DB":"l","\uFF4C":"l","\u0140":"l","\u013A":"l","\u013E":"l","\u1E37":"l","\u1E39":"l","\u013C":"l","\u1E3D":"l","\u1E3B":"l","\u017F":"l","\u0142":"l","\u019A":"l","\u026B":"l","\u2C61":"l","\uA749":"l","\uA781":"l","\uA747":"l","\u01C9":"lj","\u24DC":"m","\uFF4D":"m","\u1E3F":"m","\u1E41":"m","\u1E43":"m","\u0271":"m","\u026F":"m","\u24DD":"n","\uFF4E":"n","\u01F9":"n","\u0144":"n","\u00F1":"n","\u1E45":"n","\u0148":"n","\u1E47":"n","\u0146":"n","\u1E4B":"n","\u1E49":"n","\u019E":"n","\u0272":"n","\u0149":"n","\uA791":"n","\uA7A5":"n","\u01CC":"nj","\u24DE":"o","\uFF4F":"o","\u00F2":"o","\u00F3":"o","\u00F4":"o","\u1ED3":"o","\u1ED1":"o","\u1ED7":"o","\u1ED5":"o","\u00F5":"o","\u1E4D":"o","\u022D":"o","\u1E4F":"o","\u014D":"o","\u1E51":"o","\u1E53":"o","\u014F":"o","\u022F":"o","\u0231":"o","\u00F6":"o","\u022B":"o","\u1ECF":"o","\u0151":"o","\u01D2":"o","\u020D":"o","\u020F":"o","\u01A1":"o","\u1EDD":"o","\u1EDB":"o","\u1EE1":"o","\u1EDF":"o","\u1EE3":"o","\u1ECD":"o","\u1ED9":"o","\u01EB":"o","\u01ED":"o","\u00F8":"o","\u01FF":"o","\u0254":"o","\uA74B":"o","\uA74D":"o","\u0275":"o","\u01A3":"oi","\u0223":"ou","\uA74F":"oo","\u24DF":"p","\uFF50":"p","\u1E55":"p","\u1E57":"p","\u01A5":"p","\u1D7D":"p","\uA751":"p","\uA753":"p","\uA755":"p","\u24E0":"q","\uFF51":"q","\u024B":"q","\uA757":"q","\uA759":"q","\u24E1":"r","\uFF52":"r","\u0155":"r","\u1E59":"r","\u0159":"r","\u0211":"r","\u0213":"r","\u1E5B":"r","\u1E5D":"r","\u0157":"r","\u1E5F":"r","\u024D":"r","\u027D":"r","\uA75B":"r","\uA7A7":"r","\uA783":"r","\u24E2":"s","\uFF53":"s","\u00DF":"s","\u015B":"s","\u1E65":"s","\u015D":"s","\u1E61":"s","\u0161":"s","\u1E67":"s","\u1E63":"s","\u1E69":"s","\u0219":"s","\u015F":"s","\u023F":"s","\uA7A9":"s","\uA785":"s","\u1E9B":"s","\u24E3":"t","\uFF54":"t","\u1E6B":"t","\u1E97":"t","\u0165":"t","\u1E6D":"t","\u021B":"t","\u0163":"t","\u1E71":"t","\u1E6F":"t","\u0167":"t","\u01AD":"t","\u0288":"t","\u2C66":"t","\uA787":"t","\uA729":"tz","\u24E4":"u","\uFF55":"u","\u00F9":"u","\u00FA":"u","\u00FB":"u","\u0169":"u","\u1E79":"u","\u016B":"u","\u1E7B":"u","\u016D":"u","\u00FC":"u","\u01DC":"u","\u01D8":"u","\u01D6":"u","\u01DA":"u","\u1EE7":"u","\u016F":"u","\u0171":"u","\u01D4":"u","\u0215":"u","\u0217":"u","\u01B0":"u","\u1EEB":"u","\u1EE9":"u","\u1EEF":"u","\u1EED":"u","\u1EF1":"u","\u1EE5":"u","\u1E73":"u","\u0173":"u","\u1E77":"u","\u1E75":"u","\u0289":"u","\u24E5":"v","\uFF56":"v","\u1E7D":"v","\u1E7F":"v","\u028B":"v","\uA75F":"v","\u028C":"v","\uA761":"vy","\u24E6":"w","\uFF57":"w","\u1E81":"w","\u1E83":"w","\u0175":"w","\u1E87":"w","\u1E85":"w","\u1E98":"w","\u1E89":"w","\u2C73":"w","\u24E7":"x","\uFF58":"x","\u1E8B":"x","\u1E8D":"x","\u24E8":"y","\uFF59":"y","\u1EF3":"y","\u00FD":"y","\u0177":"y","\u1EF9":"y","\u0233":"y","\u1E8F":"y","\u00FF":"y","\u1EF7":"y","\u1E99":"y","\u1EF5":"y","\u01B4":"y","\u024F":"y","\u1EFF":"y","\u24E9":"z","\uFF5A":"z","\u017A":"z","\u1E91":"z","\u017C":"z","\u017E":"z","\u1E93":"z","\u1E95":"z","\u01B6":"z","\u0225":"z","\u0240":"z","\u2C6C":"z","\uA763":"z"};

    $document = $(document);

    nextUid=(function() { var counter=1; return function() { return counter++; }; }());


    function stripDiacritics(str) {
        var ret, i, l, c;

        if (!str || str.length < 1) return str;

        ret = "";
        for (i = 0, l = str.length; i < l; i++) {
            c = str.charAt(i);
            ret += DIACRITICS[c] || c;
        }
        return ret;
    }

    function indexOf(value, array) {
        var i = 0, l = array.length;
        for (; i < l; i = i + 1) {
            if (equal(value, array[i])) return i;
        }
        return -1;
    }

    function measureScrollbar () {
        var $template = $( MEASURE_SCROLLBAR_TEMPLATE );
        $template.appendTo('body');

        var dim = {
            width: $template.width() - $template[0].clientWidth,
            height: $template.height() - $template[0].clientHeight
        };
        $template.remove();

        return dim;
    }

    /**
     * Compares equality of a and b
     * @param a
     * @param b
     */
    function equal(a, b) {
        if (a === b) return true;
        if (a === undefined || b === undefined) return false;
        if (a === null || b === null) return false;
        // Check whether 'a' or 'b' is a string (primitive or object).
        // The concatenation of an empty string (+'') converts its argument to a string's primitive.
        if (a.constructor === String) return a+'' === b+''; // a+'' - in case 'a' is a String object
        if (b.constructor === String) return b+'' === a+''; // b+'' - in case 'b' is a String object
        return false;
    }

    /**
     * Splits the string into an array of values, trimming each value. An empty array is returned for nulls or empty
     * strings
     * @param string
     * @param separator
     */
    function splitVal(string, separator) {
        var val, i, l;
        if (string === null || string.length < 1) return [];
        val = string.split(separator);
        for (i = 0, l = val.length; i < l; i = i + 1) val[i] = $.trim(val[i]);
        return val;
    }

    function getSideBorderPadding(element) {
        return element.outerWidth(false) - element.width();
    }

    function installKeyUpChangeEvent(element) {
        var key="keyup-change-value";
        element.on("keydown", function () {
            if ($.data(element, key) === undefined) {
                $.data(element, key, element.val());
            }
        });
        element.on("keyup", function () {
            var val= $.data(element, key);
            if (val !== undefined && element.val() !== val) {
                $.removeData(element, key);
                element.trigger("keyup-change");
            }
        });
    }

    $document.on("mousemove", function (e) {
        lastMousePosition.x = e.pageX;
        lastMousePosition.y = e.pageY;
    });

    /**
     * filters mouse events so an event is fired only if the mouse moved.
     *
     * filters out mouse events that occur when mouse is stationary but
     * the elements under the pointer are scrolled.
     */
    function installFilteredMouseMove(element) {
        element.on("mousemove", function (e) {
            var lastpos = lastMousePosition;
            if (lastpos === undefined || lastpos.x !== e.pageX || lastpos.y !== e.pageY) {
                $(e.target).trigger("mousemove-filtered", e);
            }
        });
    }

    /**
     * Debounces a function. Returns a function that calls the original fn function only if no invocations have been made
     * within the last quietMillis milliseconds.
     *
     * @param quietMillis number of milliseconds to wait before invoking fn
     * @param fn function to be debounced
     * @param ctx object to be used as this reference within fn
     * @return debounced version of fn
     */
    function debounce(quietMillis, fn, ctx) {
        ctx = ctx || undefined;
        var timeout;
        return function () {
            var args = arguments;
            window.clearTimeout(timeout);
            timeout = window.setTimeout(function() {
                fn.apply(ctx, args);
            }, quietMillis);
        };
    }

    /**
     * A simple implementation of a thunk
     * @param formula function used to lazily initialize the thunk
     * @return {Function}
     */
    function thunk(formula) {
        var evaluated = false,
            value;
        return function() {
            if (evaluated === false) { value = formula(); evaluated = true; }
            return value;
        };
    };

    function installDebouncedScroll(threshold, element) {
        var notify = debounce(threshold, function (e) { element.trigger("scroll-debounced", e);});
        element.on("scroll", function (e) {
            if (indexOf(e.target, element.get()) >= 0) notify(e);
        });
    }

    function focus($el) {
        if ($el[0] === document.activeElement) return;

        /* set the focus in a 0 timeout - that way the focus is set after the processing
         of the current event has finished - which seems like the only reliable way
         to set focus */
        window.setTimeout(function() {
            var el=$el[0], pos=$el.val().length, range;

            $el.focus();

            /* make sure el received focus so we do not error out when trying to manipulate the caret.
             sometimes modals or others listeners may steal it after its set */
            if ($el.is(":visible") && el === document.activeElement) {

                /* after the focus is set move the caret to the end, necessary when we val()
                 just before setting focus */
                if(el.setSelectionRange)
                {
                    el.setSelectionRange(pos, pos);
                }
                else if (el.createTextRange) {
                    range = el.createTextRange();
                    range.collapse(false);
                    range.select();
                }
            }
        }, 0);
    }

    function getCursorInfo(el) {
        el = $(el)[0];
        var offset = 0;
        var length = 0;
        if ('selectionStart' in el) {
            offset = el.selectionStart;
            length = el.selectionEnd - offset;
        } else if ('selection' in document) {
            el.focus();
            var sel = document.selection.createRange();
            length = document.selection.createRange().text.length;
            sel.moveStart('character', -el.value.length);
            offset = sel.text.length - length;
        }
        return { offset: offset, length: length };
    }

    function killEvent(event) {
        event.preventDefault();
        event.stopPropagation();
    }
    function killEventImmediately(event) {
        event.preventDefault();
        event.stopImmediatePropagation();
    }

    function measureTextWidth(e) {
        if (!sizer){
            var style = e[0].currentStyle || window.getComputedStyle(e[0], null);
            sizer = $(document.createElement("div")).css({
                position: "absolute",
                left: "-10000px",
                top: "-10000px",
                display: "none",
                fontSize: style.fontSize,
                fontFamily: style.fontFamily,
                fontStyle: style.fontStyle,
                fontWeight: style.fontWeight,
                letterSpacing: style.letterSpacing,
                textTransform: style.textTransform,
                whiteSpace: "nowrap"
            });
            sizer.attr("class","select2-sizer");
            $("body").append(sizer);
        }
        sizer.text(e.val());
        return sizer.width();
    }

    function syncCssClasses(dest, src, adapter) {
        var classes, replacements = [], adapted;

        classes = dest.attr("class");
        if (classes) {
            classes = '' + classes; // for IE which returns object
            $(classes.split(" ")).each2(function() {
                if (this.indexOf("select2-") === 0) {
                    replacements.push(this);
                }
            });
        }
        classes = src.attr("class");
        if (classes) {
            classes = '' + classes; // for IE which returns object
            $(classes.split(" ")).each2(function() {
                if (this.indexOf("select2-") !== 0) {
                    adapted = adapter(this);
                    if (adapted) {
                        replacements.push(adapted);
                    }
                }
            });
        }
        dest.attr("class", replacements.join(" "));
    }


    function markMatch(text, term, markup, escapeMarkup) {
        var match=stripDiacritics(text.toUpperCase()).indexOf(stripDiacritics(term.toUpperCase())),
            tl=term.length;

        if (match<0) {
            markup.push(escapeMarkup(text));
            return;
        }

        markup.push(escapeMarkup(text.substring(0, match)));
        markup.push("<span class='select2-match'>");
        markup.push(escapeMarkup(text.substring(match, match + tl)));
        markup.push("</span>");
        markup.push(escapeMarkup(text.substring(match + tl, text.length)));
    }

    function defaultEscapeMarkup(markup) {
        var replace_map = {
            '\\': '&#92;',
            '&': '&amp;',
            '<': '&lt;',
            '>': '&gt;',
            '"': '&quot;',
            "'": '&#39;',
            "/": '&#47;'
        };

        return String(markup).replace(/[&<>"'\/\\]/g, function (match) {
            return replace_map[match];
        });
    }

    /**
     * Produces an ajax-based query function
     *
     * @param options object containing configuration paramters
     * @param options.params parameter map for the transport ajax call, can contain such options as cache, jsonpCallback, etc. see $.ajax
     * @param options.transport function that will be used to execute the ajax request. must be compatible with parameters supported by $.ajax
     * @param options.url url for the data
     * @param options.data a function(searchTerm, pageNumber, context) that should return an object containing query string parameters for the above url.
     * @param options.dataType request data type: ajax, jsonp, other datatatypes supported by jQuery's $.ajax function or the transport function if specified
     * @param options.quietMillis (optional) milliseconds to wait before making the ajaxRequest, helps debounce the ajax function if invoked too often
     * @param options.results a function(remoteData, pageNumber) that converts data returned form the remote request to the format expected by Select2.
     *      The expected format is an object containing the following keys:
     *      results array of objects that will be used as choices
     *      more (optional) boolean indicating whether there are more results available
     *      Example: {results:[{id:1, text:'Red'},{id:2, text:'Blue'}], more:true}
     */
    function ajax(options) {
        var timeout, // current scheduled but not yet executed request
            handler = null,
            quietMillis = options.quietMillis || 100,
            ajaxUrl = options.url,
            self = this;

        return function (query) {
            window.clearTimeout(timeout);
            timeout = window.setTimeout(function () {
                var data = options.data, // ajax data function
                    url = ajaxUrl, // ajax url string or function
                    transport = options.transport || $.fn.select2.ajaxDefaults.transport,
                // deprecated - to be removed in 4.0  - use params instead
                    deprecated = {
                        type: options.type || 'GET', // set type of request (GET or POST)
                        cache: options.cache || false,
                        jsonpCallback: options.jsonpCallback||undefined,
                        dataType: options.dataType||"json"
                    },
                    params = $.extend({}, $.fn.select2.ajaxDefaults.params, deprecated);

                data = data ? data.call(self, query.term, query.page, query.context) : null;
                url = (typeof url === 'function') ? url.call(self, query.term, query.page, query.context) : url;

                if (handler) { handler.abort(); }

                if (options.params) {
                    if ($.isFunction(options.params)) {
                        $.extend(params, options.params.call(self));
                    } else {
                        $.extend(params, options.params);
                    }
                }

                $.extend(params, {
                    url: url,
                    dataType: options.dataType,
                    data: data,
                    success: function (data) {
                        // TODO - replace query.page with query so users have access to term, page, etc.
                        var results = options.results(data, query.page);
                        query.callback(results);
                    }
                });
                handler = transport.call(self, params);
            }, quietMillis);
        };
    }

    /**
     * Produces a query function that works with a local array
     *
     * @param options object containing configuration parameters. The options parameter can either be an array or an
     * object.
     *
     * If the array form is used it is assumed that it contains objects with 'id' and 'text' keys.
     *
     * If the object form is used ti is assumed that it contains 'data' and 'text' keys. The 'data' key should contain
     * an array of objects that will be used as choices. These objects must contain at least an 'id' key. The 'text'
     * key can either be a String in which case it is expected that each element in the 'data' array has a key with the
     * value of 'text' which will be used to match choices. Alternatively, text can be a function(item) that can extract
     * the text.
     */
    function local(options) {
        var data = options, // data elements
            dataText,
            tmp,
            text = function (item) { return ""+item.text; }; // function used to retrieve the text portion of a data item that is matched against the search

        if ($.isArray(data)) {
            tmp = data;
            data = { results: tmp };
        }

        if ($.isFunction(data) === false) {
            tmp = data;
            data = function() { return tmp; };
        }

        var dataItem = data();
        if (dataItem.text) {
            text = dataItem.text;
            // if text is not a function we assume it to be a key name
            if (!$.isFunction(text)) {
                dataText = dataItem.text; // we need to store this in a separate variable because in the next step data gets reset and data.text is no longer available
                text = function (item) { return item[dataText]; };
            }
        }

        return function (query) {
            var t = query.term, filtered = { results: [] }, process;
            if (t === "") {
                query.callback(data());
                return;
            }

            process = function(datum, collection) {
                var group, attr;
                datum = datum[0];
                if (datum.children) {
                    group = {};
                    for (attr in datum) {
                        if (datum.hasOwnProperty(attr)) group[attr]=datum[attr];
                    }
                    group.children=[];
                    $(datum.children).each2(function(i, childDatum) { process(childDatum, group.children); });
                    if (group.children.length || query.matcher(t, text(group), datum)) {
                        collection.push(group);
                    }
                } else {
                    if (query.matcher(t, text(datum), datum)) {
                        collection.push(datum);
                    }
                }
            };

            $(data().results).each2(function(i, datum) { process(datum, filtered.results); });
            query.callback(filtered);
        };
    }

    // TODO javadoc
    function tags(data) {
        var isFunc = $.isFunction(data);
        return function (query) {
            var t = query.term, filtered = {results: []};
            $(isFunc ? data() : data).each(function () {
                var isObject = this.text !== undefined,
                    text = isObject ? this.text : this;
                if (t === "" || query.matcher(t, text)) {
                    filtered.results.push(isObject ? this : {id: this, text: this});
                }
            });
            query.callback(filtered);
        };
    }

    /**
     * Checks if the formatter function should be used.
     *
     * Throws an error if it is not a function. Returns true if it should be used,
     * false if no formatting should be performed.
     *
     * @param formatter
     */
    function checkFormatter(formatter, formatterName) {
        if ($.isFunction(formatter)) return true;
        if (!formatter) return false;
        throw new Error(formatterName +" must be a function or a falsy value");
    }

    function evaluate(val) {
        return $.isFunction(val) ? val() : val;
    }

    function countResults(results) {
        var count = 0;
        $.each(results, function(i, item) {
            if (item.children) {
                count += countResults(item.children);
            } else {
                count++;
            }
        });
        return count;
    }

    /**
     * Default tokenizer. This function uses breaks the input on substring match of any string from the
     * opts.tokenSeparators array and uses opts.createSearchChoice to create the choice object. Both of those
     * two options have to be defined in order for the tokenizer to work.
     *
     * @param input text user has typed so far or pasted into the search field
     * @param selection currently selected choices
     * @param selectCallback function(choice) callback tho add the choice to selection
     * @param opts select2's opts
     * @return undefined/null to leave the current input unchanged, or a string to change the input to the returned value
     */
    function defaultTokenizer(input, selection, selectCallback, opts) {
        var original = input, // store the original so we can compare and know if we need to tell the search to update its text
            dupe = false, // check for whether a token we extracted represents a duplicate selected choice
            token, // token
            index, // position at which the separator was found
            i, l, // looping variables
            separator; // the matched separator

        if (!opts.createSearchChoice || !opts.tokenSeparators || opts.tokenSeparators.length < 1) return undefined;

        while (true) {
            index = -1;

            for (i = 0, l = opts.tokenSeparators.length; i < l; i++) {
                separator = opts.tokenSeparators[i];
                index = input.indexOf(separator);
                if (index >= 0) break;
            }

            if (index < 0) break; // did not find any token separator in the input string, bail

            token = input.substring(0, index);
            input = input.substring(index + separator.length);

            if (token.length > 0) {
                token = opts.createSearchChoice.call(this, token, selection);
                if (token !== undefined && token !== null && opts.id(token) !== undefined && opts.id(token) !== null) {
                    dupe = false;
                    for (i = 0, l = selection.length; i < l; i++) {
                        if (equal(opts.id(token), opts.id(selection[i]))) {
                            dupe = true; break;
                        }
                    }

                    if (!dupe) selectCallback(token);
                }
            }
        }

        if (original!==input) return input;
    }

    /**
     * Creates a new class
     *
     * @param superClass
     * @param methods
     */
    function clazz(SuperClass, methods) {
        var constructor = function () {};
        constructor.prototype = new SuperClass;
        constructor.prototype.constructor = constructor;
        constructor.prototype.parent = SuperClass.prototype;
        constructor.prototype = $.extend(constructor.prototype, methods);
        return constructor;
    }

    AbstractSelect2 = clazz(Object, {

        // abstract
        bind: function (func) {
            var self = this;
            return function () {
                func.apply(self, arguments);
            };
        },

        // abstract
        init: function (opts) {
            var results, search, resultsSelector = ".select2-results";

            // prepare options
            this.opts = opts = this.prepareOpts(opts);

            this.id=opts.id;

            // destroy if called on an existing component
            if (opts.element.data("select2") !== undefined &&
                opts.element.data("select2") !== null) {
                opts.element.data("select2").destroy();
            }

            this.container = this.createContainer();

            this.containerId="s2id_"+(opts.element.attr("id") || "autogen"+nextUid());
            this.containerSelector="#"+this.containerId.replace(/([;&,\.\+\*\~':"\!\^#$%@\[\]\(\)=>\|])/g, '\\$1');
            this.container.attr("id", this.containerId);

            // cache the body so future lookups are cheap
            this.body = thunk(function() { return opts.element.closest("body"); });

            syncCssClasses(this.container, this.opts.element, this.opts.adaptContainerCssClass);

            this.container.attr("style", opts.element.attr("style"));
            this.container.css(evaluate(opts.containerCss));
            this.container.addClass(evaluate(opts.containerCssClass));

            this.elementTabIndex = this.opts.element.attr("tabindex");

            // swap container for the element
            this.opts.element
                .data("select2", this)
                .attr("tabindex", "-1")
                .before(this.container)
                .on("click.select2", killEvent); // do not leak click events

            this.container.data("select2", this);

            this.dropdown = this.container.find(".select2-drop");

            syncCssClasses(this.dropdown, this.opts.element, this.opts.adaptDropdownCssClass);

            this.dropdown.addClass(evaluate(opts.dropdownCssClass));
            this.dropdown.data("select2", this);
            this.dropdown.on("click", killEvent);

            this.results = results = this.container.find(resultsSelector);
            this.search = search = this.container.find("input.select2-input");

            this.queryCount = 0;
            this.resultsPage = 0;
            this.context = null;

            // initialize the container
            this.initContainer();

            this.container.on("click", killEvent);

            installFilteredMouseMove(this.results);
            this.dropdown.on("mousemove-filtered touchstart touchmove touchend", resultsSelector, this.bind(this.highlightUnderEvent));

            installDebouncedScroll(80, this.results);
            this.dropdown.on("scroll-debounced", resultsSelector, this.bind(this.loadMoreIfNeeded));

            // do not propagate change event from the search field out of the component
            $(this.container).on("change", ".select2-input", function(e) {e.stopPropagation();});
            $(this.dropdown).on("change", ".select2-input", function(e) {e.stopPropagation();});

            // if jquery.mousewheel plugin is installed we can prevent out-of-bounds scrolling of results via mousewheel
            if ($.fn.mousewheel) {
                results.mousewheel(function (e, delta, deltaX, deltaY) {
                    var top = results.scrollTop();
                    if (deltaY > 0 && top - deltaY <= 0) {
                        results.scrollTop(0);
                        killEvent(e);
                    } else if (deltaY < 0 && results.get(0).scrollHeight - results.scrollTop() + deltaY <= results.height()) {
                        results.scrollTop(results.get(0).scrollHeight - results.height());
                        killEvent(e);
                    }
                });
            }

            installKeyUpChangeEvent(search);
            search.on("keyup-change input paste", this.bind(this.updateResults));
            search.on("focus", function () { search.addClass("select2-focused"); });
            search.on("blur", function () { search.removeClass("select2-focused");});

            this.dropdown.on("mouseup", resultsSelector, this.bind(function (e) {
                if ($(e.target).closest(".select2-result-selectable").length > 0) {
                    this.highlightUnderEvent(e);
                    this.selectHighlighted(e);
                }
            }));

            // trap all mouse events from leaving the dropdown. sometimes there may be a modal that is listening
            // for mouse events outside of itself so it can close itself. since the dropdown is now outside the select2's
            // dom it will trigger the popup close, which is not what we want
            this.dropdown.on("click mouseup mousedown", function (e) { e.stopPropagation(); });

            if ($.isFunction(this.opts.initSelection)) {
                // initialize selection based on the current value of the source element
                this.initSelection();

                // if the user has provided a function that can set selection based on the value of the source element
                // we monitor the change event on the element and trigger it, allowing for two way synchronization
                this.monitorSource();
            }

            if (opts.maximumInputLength !== null) {
                this.search.attr("maxlength", opts.maximumInputLength);
            }

            var disabled = opts.element.prop("disabled");
            if (disabled === undefined) disabled = false;
            this.enable(!disabled);

            var readonly = opts.element.prop("readonly");
            if (readonly === undefined) readonly = false;
            this.readonly(readonly);

            // Calculate size of scrollbar
            scrollBarDimensions = scrollBarDimensions || measureScrollbar();

            this.autofocus = opts.element.prop("autofocus");
            opts.element.prop("autofocus", false);
            if (this.autofocus) this.focus();

            this.nextSearchTerm = undefined;
        },

        // abstract
        destroy: function () {
            var element=this.opts.element, select2 = element.data("select2");

            this.close();

            if (this.propertyObserver) { delete this.propertyObserver; this.propertyObserver = null; }

            if (select2 !== undefined) {
                select2.container.remove();
                select2.dropdown.remove();
                element
                    .removeClass("select2-offscreen")
                    .removeData("select2")
                    .off(".select2")
                    .prop("autofocus", this.autofocus || false);
                if (this.elementTabIndex) {
                    element.attr({tabindex: this.elementTabIndex});
                } else {
                    element.removeAttr("tabindex");
                }
                element.show();
            }
        },

        // abstract
        optionToData: function(element) {
            if (element.is("option")) {
                return {
                    id:element.prop("value"),
                    text:element.text(),
                    element: element.get(),
                    css: element.attr("class"),
                    disabled: element.prop("disabled"),
                    locked: equal(element.attr("locked"), "locked") || equal(element.data("locked"), true)
                };
            } else if (element.is("optgroup")) {
                return {
                    text:element.attr("label"),
                    children:[],
                    element: element.get(),
                    css: element.attr("class")
                };
            }
        },

        // abstract
        prepareOpts: function (opts) {
            var element, select, idKey, ajaxUrl, self = this;

            element = opts.element;

            if (element.get(0).tagName.toLowerCase() === "select") {
                this.select = select = opts.element;
            }

            if (select) {
                // these options are not allowed when attached to a select because they are picked up off the element itself
                $.each(["id", "multiple", "ajax", "query", "createSearchChoice", "initSelection", "data", "tags"], function () {
                    if (this in opts) {
                        throw new Error("Option '" + this + "' is not allowed for Select2 when attached to a <select> element.");
                    }
                });
            }

            opts = $.extend({}, {
                populateResults: function(container, results, query) {
                    var populate, id=this.opts.id;

                    populate=function(results, container, depth) {

                        var i, l, result, selectable, disabled, compound, node, label, innerContainer, formatted;

                        results = opts.sortResults(results, container, query);

                        for (i = 0, l = results.length; i < l; i = i + 1) {

                            result=results[i];

                            disabled = (result.disabled === true);
                            selectable = (!disabled) && (id(result) !== undefined);

                            compound=result.children && result.children.length > 0;

                            node=$("<li></li>");
                            node.addClass("select2-results-dept-"+depth);
                            node.addClass("select2-result");
                            node.addClass(selectable ? "select2-result-selectable" : "select2-result-unselectable");
                            if (disabled) { node.addClass("select2-disabled"); }
                            if (compound) { node.addClass("select2-result-with-children"); }
                            node.addClass(self.opts.formatResultCssClass(result));

                            label=$(document.createElement("div"));
                            label.addClass("select2-result-label");

                            formatted=opts.formatResult(result, label, query, self.opts.escapeMarkup);
                            if (formatted!==undefined) {
                                label.html(formatted);
                            }

                            node.append(label);

                            if (compound) {

                                innerContainer=$("<ul></ul>");
                                innerContainer.addClass("select2-result-sub");
                                populate(result.children, innerContainer, depth+1);
                                node.append(innerContainer);
                            }

                            node.data("select2-data", result);
                            container.append(node);
                        }
                    };

                    populate(results, container, 0);
                }
            }, $.fn.select2.defaults, opts);

            if (typeof(opts.id) !== "function") {
                idKey = opts.id;
                opts.id = function (e) { return e[idKey]; };
            }

            if ($.isArray(opts.element.data("select2Tags"))) {
                if ("tags" in opts) {
                    throw "tags specified as both an attribute 'data-select2-tags' and in options of Select2 " + opts.element.attr("id");
                }
                opts.tags=opts.element.data("select2Tags");
            }

            if (select) {
                opts.query = this.bind(function (query) {
                    var data = { results: [], more: false },
                        term = query.term,
                        children, placeholderOption, process;

                    process=function(element, collection) {
                        var group;
                        if (element.is("option")) {
                            if (query.matcher(term, element.text(), element)) {
                                collection.push(self.optionToData(element));
                            }
                        } else if (element.is("optgroup")) {
                            group=self.optionToData(element);
                            element.children().each2(function(i, elm) { process(elm, group.children); });
                            if (group.children.length>0) {
                                collection.push(group);
                            }
                        }
                    };

                    children=element.children();

                    // ignore the placeholder option if there is one
                    if (this.getPlaceholder() !== undefined && children.length > 0) {
                        placeholderOption = this.getPlaceholderOption();
                        if (placeholderOption) {
                            children=children.not(placeholderOption);
                        }
                    }

                    children.each2(function(i, elm) { process(elm, data.results); });

                    query.callback(data);
                });
                // this is needed because inside val() we construct choices from options and there id is hardcoded
                opts.id=function(e) { return e.id; };
                opts.formatResultCssClass = function(data) { return data.css; };
            } else {
                if (!("query" in opts)) {

                    if ("ajax" in opts) {
                        ajaxUrl = opts.element.data("ajax-url");
                        if (ajaxUrl && ajaxUrl.length > 0) {
                            opts.ajax.url = ajaxUrl;
                        }
                        opts.query = ajax.call(opts.element, opts.ajax);
                    } else if ("data" in opts) {
                        opts.query = local(opts.data);
                    } else if ("tags" in opts) {
                        opts.query = tags(opts.tags);
                        if (opts.createSearchChoice === undefined) {
                            opts.createSearchChoice = function (term) { return {id: $.trim(term), text: $.trim(term)}; };
                        }
                        if (opts.initSelection === undefined) {
                            opts.initSelection = function (element, callback) {
                                var data = [];
                                $(splitVal(element.val(), opts.separator)).each(function () {
                                    var obj = { id: this, text: this },
                                        tags = opts.tags;
                                    if ($.isFunction(tags)) tags=tags();
                                    $(tags).each(function() { if (equal(this.id, obj.id)) { obj = this; return false; } });
                                    data.push(obj);
                                });

                                callback(data);
                            };
                        }
                    }
                }
            }
            if (typeof(opts.query) !== "function") {
                throw "query function not defined for Select2 " + opts.element.attr("id");
            }

            return opts;
        },

        /**
         * Monitor the original element for changes and update select2 accordingly
         */
        // abstract
        monitorSource: function () {
            var el = this.opts.element, sync, observer;

            el.on("change.select2", this.bind(function (e) {
                if (this.opts.element.data("select2-change-triggered") !== true) {
                    this.initSelection();
                }
            }));

            sync = this.bind(function () {

                // sync enabled state
                var disabled = el.prop("disabled");
                if (disabled === undefined) disabled = false;
                this.enable(!disabled);

                var readonly = el.prop("readonly");
                if (readonly === undefined) readonly = false;
                this.readonly(readonly);

                syncCssClasses(this.container, this.opts.element, this.opts.adaptContainerCssClass);
                this.container.addClass(evaluate(this.opts.containerCssClass));

                syncCssClasses(this.dropdown, this.opts.element, this.opts.adaptDropdownCssClass);
                this.dropdown.addClass(evaluate(this.opts.dropdownCssClass));

            });

            // IE8-10
            el.on("propertychange.select2", sync);

            // hold onto a reference of the callback to work around a chromium bug
            if (this.mutationCallback === undefined) {
                this.mutationCallback = function (mutations) {
                    mutations.forEach(sync);
                }
            }

            // safari, chrome, firefox, IE11
            observer = window.MutationObserver || window.WebKitMutationObserver|| window.MozMutationObserver;
            if (observer !== undefined) {
                if (this.propertyObserver) { delete this.propertyObserver; this.propertyObserver = null; }
                this.propertyObserver = new observer(this.mutationCallback);
                this.propertyObserver.observe(el.get(0), { attributes:true, subtree:false });
            }
        },

        // abstract
        triggerSelect: function(data) {
            var evt = $.Event("select2-selecting", { val: this.id(data), object: data });
            this.opts.element.trigger(evt);
            return !evt.isDefaultPrevented();
        },

        /**
         * Triggers the change event on the source element
         */
        // abstract
        triggerChange: function (details) {

            details = details || {};
            details= $.extend({}, details, { type: "change", val: this.val() });
            // prevents recursive triggering
            this.opts.element.data("select2-change-triggered", true);
            this.opts.element.trigger(details);
            this.opts.element.data("select2-change-triggered", false);

            // some validation frameworks ignore the change event and listen instead to keyup, click for selects
            // so here we trigger the click event manually
            this.opts.element.click();

            // ValidationEngine ignorea the change event and listens instead to blur
            // so here we trigger the blur event manually if so desired
            if (this.opts.blurOnChange)
                this.opts.element.blur();
        },

        //abstract
        isInterfaceEnabled: function()
        {
            return this.enabledInterface === true;
        },

        // abstract
        enableInterface: function() {
            var enabled = this._enabled && !this._readonly,
                disabled = !enabled;

            if (enabled === this.enabledInterface) return false;

            this.container.toggleClass("select2-container-disabled", disabled);
            this.close();
            this.enabledInterface = enabled;

            return true;
        },

        // abstract
        enable: function(enabled) {
            if (enabled === undefined) enabled = true;
            if (this._enabled === enabled) return;
            this._enabled = enabled;

            this.opts.element.prop("disabled", !enabled);
            this.enableInterface();
        },

        // abstract
        disable: function() {
            this.enable(false);
        },

        // abstract
        readonly: function(enabled) {
            if (enabled === undefined) enabled = false;
            if (this._readonly === enabled) return false;
            this._readonly = enabled;

            this.opts.element.prop("readonly", enabled);
            this.enableInterface();
            return true;
        },

        // abstract
        opened: function () {
            return this.container.hasClass("select2-dropdown-open");
        },

        // abstract
        positionDropdown: function() {
            var $dropdown = this.dropdown,
                offset = this.container.offset(),
                height = this.container.outerHeight(false),
                width = this.container.outerWidth(false),
                dropHeight = $dropdown.outerHeight(false),
                $window = $(window),
                windowWidth = $window.width(),
                windowHeight = $window.height(),
                viewPortRight = $window.scrollLeft() + windowWidth,
                viewportBottom = $window.scrollTop() + windowHeight,
                dropTop = offset.top + height,
                dropLeft = offset.left,
                enoughRoomBelow = dropTop + dropHeight <= viewportBottom,
                enoughRoomAbove = (offset.top - dropHeight) >= this.body().scrollTop(),
                dropWidth = $dropdown.outerWidth(false),
                enoughRoomOnRight = dropLeft + dropWidth <= viewPortRight,
                aboveNow = $dropdown.hasClass("select2-drop-above"),
                bodyOffset,
                above,
                changeDirection,
                css,
                resultsListNode;

            // always prefer the current above/below alignment, unless there is not enough room
            if (aboveNow) {
                above = true;
                if (!enoughRoomAbove && enoughRoomBelow) {
                    changeDirection = true;
                    above = false;
                }
            } else {
                above = false;
                if (!enoughRoomBelow && enoughRoomAbove) {
                    changeDirection = true;
                    above = true;
                }
            }

            //if we are changing direction we need to get positions when dropdown is hidden;
            if (changeDirection) {
                $dropdown.hide();
                offset = this.container.offset();
                height = this.container.outerHeight(false);
                width = this.container.outerWidth(false);
                dropHeight = $dropdown.outerHeight(false);
                viewPortRight = $window.scrollLeft() + windowWidth;
                viewportBottom = $window.scrollTop() + windowHeight;
                dropTop = offset.top + height;
                dropLeft = offset.left;
                dropWidth = $dropdown.outerWidth(false);
                enoughRoomOnRight = dropLeft + dropWidth <= viewPortRight;
                $dropdown.show();
            }

            if (this.opts.dropdownAutoWidth) {
                resultsListNode = $('.select2-results', $dropdown)[0];
                $dropdown.addClass('select2-drop-auto-width');
                $dropdown.css('width', '');
                // Add scrollbar width to dropdown if vertical scrollbar is present
                dropWidth = $dropdown.outerWidth(false) + (resultsListNode.scrollHeight === resultsListNode.clientHeight ? 0 : scrollBarDimensions.width);
                dropWidth > width ? width = dropWidth : dropWidth = width;
                enoughRoomOnRight = dropLeft + dropWidth <= viewPortRight;
            }
            else {
                this.container.removeClass('select2-drop-auto-width');
            }

            //console.log("below/ droptop:", dropTop, "dropHeight", dropHeight, "sum", (dropTop+dropHeight)+" viewport bottom", viewportBottom, "enough?", enoughRoomBelow);
            //console.log("above/ offset.top", offset.top, "dropHeight", dropHeight, "top", (offset.top-dropHeight), "scrollTop", this.body().scrollTop(), "enough?", enoughRoomAbove);

            // fix positioning when body has an offset and is not position: static
            if (this.body().css('position') !== 'static') {
                bodyOffset = this.body().offset();
                dropTop -= bodyOffset.top;
                dropLeft -= bodyOffset.left;
            }

            if (!enoughRoomOnRight) {
                dropLeft = offset.left + width - dropWidth;
            }

            css =  {
                left: dropLeft,
                width: width
            };

            if (above) {
                css.bottom = windowHeight - offset.top;
                css.top = 'auto';
                this.container.addClass("select2-drop-above");
                $dropdown.addClass("select2-drop-above");
            }
            else {
                css.top = dropTop;
                css.bottom = 'auto';
                this.container.removeClass("select2-drop-above");
                $dropdown.removeClass("select2-drop-above");
            }
            css = $.extend(css, evaluate(this.opts.dropdownCss));

            $dropdown.css(css);
        },

        // abstract
        shouldOpen: function() {
            var event;

            if (this.opened()) return false;

            if (this._enabled === false || this._readonly === true) return false;

            event = $.Event("select2-opening");
            this.opts.element.trigger(event);
            return !event.isDefaultPrevented();
        },

        // abstract
        clearDropdownAlignmentPreference: function() {
            // clear the classes used to figure out the preference of where the dropdown should be opened
            this.container.removeClass("select2-drop-above");
            this.dropdown.removeClass("select2-drop-above");
        },

        /**
         * Opens the dropdown
         *
         * @return {Boolean} whether or not dropdown was opened. This method will return false if, for example,
         * the dropdown is already open, or if the 'open' event listener on the element called preventDefault().
         */
        // abstract
        open: function () {

            if (!this.shouldOpen()) return false;

            this.opening();

            return true;
        },

        /**
         * Performs the opening of the dropdown
         */
        // abstract
        opening: function() {
            var cid = this.containerId,
                scroll = "scroll." + cid,
                resize = "resize."+cid,
                orient = "orientationchange."+cid,
                mask;

            this.container.addClass("select2-dropdown-open").addClass("select2-container-active");

            this.clearDropdownAlignmentPreference();

            if(this.dropdown[0] !== this.body().children().last()[0]) {
                this.dropdown.detach().appendTo(this.body());
            }

            // create the dropdown mask if doesnt already exist
            mask = $("#select2-drop-mask");
            if (mask.length == 0) {
                mask = $(document.createElement("div"));
                mask.attr("id","select2-drop-mask").attr("class","select2-drop-mask");
                mask.hide();
                mask.appendTo(this.body());
                mask.on("mousedown touchstart click", function (e) {
                    var dropdown = $("#select2-drop"), self;
                    if (dropdown.length > 0) {
                        self=dropdown.data("select2");
                        if (self.opts.selectOnBlur) {
                            self.selectHighlighted({noFocus: true});
                        }
                        self.close({focus:true});
                        e.preventDefault();
                        e.stopPropagation();
                    }
                });
            }

            // ensure the mask is always right before the dropdown
            if (this.dropdown.prev()[0] !== mask[0]) {
                this.dropdown.before(mask);
            }

            // move the global id to the correct dropdown
            $("#select2-drop").removeAttr("id");
            this.dropdown.attr("id", "select2-drop");

            // show the elements
            mask.show();

            this.positionDropdown();
            this.dropdown.show();
            this.positionDropdown();

            this.dropdown.addClass("select2-drop-active");

            // attach listeners to events that can change the position of the container and thus require
            // the position of the dropdown to be updated as well so it does not come unglued from the container
            var that = this;
            this.container.parents().add(window).each(function () {
                $(this).on(resize+" "+scroll+" "+orient, function (e) {
                    that.positionDropdown();
                });
            });


        },

        // abstract
        close: function () {
            if (!this.opened()) return;

            var cid = this.containerId,
                scroll = "scroll." + cid,
                resize = "resize."+cid,
                orient = "orientationchange."+cid;

            // unbind event listeners
            this.container.parents().add(window).each(function () { $(this).off(scroll).off(resize).off(orient); });

            this.clearDropdownAlignmentPreference();

            $("#select2-drop-mask").hide();
            this.dropdown.removeAttr("id"); // only the active dropdown has the select2-drop id
            this.dropdown.hide();
            this.container.removeClass("select2-dropdown-open").removeClass("select2-container-active");
            this.results.empty();


            this.clearSearch();
            this.search.removeClass("select2-active");
            this.opts.element.trigger($.Event("select2-close"));
        },

        /**
         * Opens control, sets input value, and updates results.
         */
        // abstract
        externalSearch: function (term) {
            this.open();
            this.search.val(term);
            this.updateResults(false);
        },

        // abstract
        clearSearch: function () {

        },

        //abstract
        getMaximumSelectionSize: function() {
            return evaluate(this.opts.maximumSelectionSize);
        },

        // abstract
        ensureHighlightVisible: function () {
            var results = this.results, children, index, child, hb, rb, y, more;

            index = this.highlight();

            if (index < 0) return;

            if (index == 0) {

                // if the first element is highlighted scroll all the way to the top,
                // that way any unselectable headers above it will also be scrolled
                // into view

                results.scrollTop(0);
                return;
            }

            children = this.findHighlightableChoices().find('.select2-result-label');

            child = $(children[index]);

            hb = child.offset().top + child.outerHeight(true);

            // if this is the last child lets also make sure select2-more-results is visible
            if (index === children.length - 1) {
                more = results.find("li.select2-more-results");
                if (more.length > 0) {
                    hb = more.offset().top + more.outerHeight(true);
                }
            }

            rb = results.offset().top + results.outerHeight(true);
            if (hb > rb) {
                results.scrollTop(results.scrollTop() + (hb - rb));
            }
            y = child.offset().top - results.offset().top;

            // make sure the top of the element is visible
            if (y < 0 && child.css('display') != 'none' ) {
                results.scrollTop(results.scrollTop() + y); // y is negative
            }
        },

        // abstract
        findHighlightableChoices: function() {
            return this.results.find(".select2-result-selectable:not(.select2-disabled, .select2-selected)");
        },

        // abstract
        moveHighlight: function (delta) {
            var choices = this.findHighlightableChoices(),
                index = this.highlight();

            while (index > -1 && index < choices.length) {
                index += delta;
                var choice = $(choices[index]);
                if (choice.hasClass("select2-result-selectable") && !choice.hasClass("select2-disabled") && !choice.hasClass("select2-selected")) {
                    this.highlight(index);
                    break;
                }
            }
        },

        // abstract
        highlight: function (index) {
            var choices = this.findHighlightableChoices(),
                choice,
                data;

            if (arguments.length === 0) {
                return indexOf(choices.filter(".select2-highlighted")[0], choices.get());
            }

            if (index >= choices.length) index = choices.length - 1;
            if (index < 0) index = 0;

            this.removeHighlight();

            choice = $(choices[index]);
            choice.addClass("select2-highlighted");

            this.ensureHighlightVisible();

            data = choice.data("select2-data");
            if (data) {
                this.opts.element.trigger({ type: "select2-highlight", val: this.id(data), choice: data });
            }
        },

        removeHighlight: function() {
            this.results.find(".select2-highlighted").removeClass("select2-highlighted");
        },

        // abstract
        countSelectableResults: function() {
            return this.findHighlightableChoices().length;
        },

        // abstract
        highlightUnderEvent: function (event) {
            var el = $(event.target).closest(".select2-result-selectable");
            if (el.length > 0 && !el.is(".select2-highlighted")) {
                var choices = this.findHighlightableChoices();
                this.highlight(choices.index(el));
            } else if (el.length == 0) {
                // if we are over an unselectable item remove all highlights
                this.removeHighlight();
            }
        },

        // abstract
        loadMoreIfNeeded: function () {
            var results = this.results,
                more = results.find("li.select2-more-results"),
                below, // pixels the element is below the scroll fold, below==0 is when the element is starting to be visible
                page = this.resultsPage + 1,
                self=this,
                term=this.search.val(),
                context=this.context;

            if (more.length === 0) return;
            below = more.offset().top - results.offset().top - results.height();

            if (below <= this.opts.loadMorePadding) {
                more.addClass("select2-active");
                this.opts.query({
                    element: this.opts.element,
                    term: term,
                    page: page,
                    context: context,
                    matcher: this.opts.matcher,
                    callback: this.bind(function (data) {

                        // ignore a response if the select2 has been closed before it was received
                        if (!self.opened()) return;


                        self.opts.populateResults.call(this, results, data.results, {term: term, page: page, context:context});
                        self.postprocessResults(data, false, false);

                        if (data.more===true) {
                            more.detach().appendTo(results).text(self.opts.formatLoadMore(page+1));
                            window.setTimeout(function() { self.loadMoreIfNeeded(); }, 10);
                        } else {
                            more.remove();
                        }
                        self.positionDropdown();
                        self.resultsPage = page;
                        self.context = data.context;
                        this.opts.element.trigger({ type: "select2-loaded", items: data });
                    })});
            }
        },

        /**
         * Default tokenizer function which does nothing
         */
        tokenize: function() {

        },

        /**
         * @param initial whether or not this is the call to this method right after the dropdown has been opened
         */
        // abstract
        updateResults: function (initial) {
            var search = this.search,
                results = this.results,
                opts = this.opts,
                data,
                self = this,
                input,
                term = search.val(),
                lastTerm = $.data(this.container, "select2-last-term"),
            // sequence number used to drop out-of-order responses
                queryNumber;

            // prevent duplicate queries against the same term
            if (initial !== true && lastTerm && equal(term, lastTerm)) return;

            $.data(this.container, "select2-last-term", term);

            // if the search is currently hidden we do not alter the results
            if (initial !== true && (this.showSearchInput === false || !this.opened())) {
                return;
            }

            function postRender() {
                search.removeClass("select2-active");
                self.positionDropdown();
            }

            function render(html) {
                results.html(html);
                postRender();
            }

            queryNumber = ++this.queryCount;

            var maxSelSize = this.getMaximumSelectionSize();
            if (maxSelSize >=1) {
                data = this.data();
                if ($.isArray(data) && data.length >= maxSelSize && checkFormatter(opts.formatSelectionTooBig, "formatSelectionTooBig")) {
                    render("<li class='select2-selection-limit'>" + opts.formatSelectionTooBig(maxSelSize) + "</li>");
                    return;
                }
            }

            if (search.val().length < opts.minimumInputLength) {
                if (checkFormatter(opts.formatInputTooShort, "formatInputTooShort")) {
                    render("<li class='select2-no-results'>" + opts.formatInputTooShort(search.val(), opts.minimumInputLength) + "</li>");
                } else {
                    render("");
                }
                if (initial && this.showSearch) this.showSearch(true);
                return;
            }

            if (opts.maximumInputLength && search.val().length > opts.maximumInputLength) {
                if (checkFormatter(opts.formatInputTooLong, "formatInputTooLong")) {
                    render("<li class='select2-no-results'>" + opts.formatInputTooLong(search.val(), opts.maximumInputLength) + "</li>");
                } else {
                    render("");
                }
                return;
            }

            if (opts.formatSearching && this.findHighlightableChoices().length === 0) {
                render("<li class='select2-searching'>" + opts.formatSearching() + "</li>");
            }

            search.addClass("select2-active");

            this.removeHighlight();

            // give the tokenizer a chance to pre-process the input
            input = this.tokenize();
            if (input != undefined && input != null) {
                search.val(input);
            }

            this.resultsPage = 1;

            opts.query({
                element: opts.element,
                term: search.val(),
                page: this.resultsPage,
                context: null,
                matcher: opts.matcher,
                callback: this.bind(function (data) {
                    var def; // default choice

                    // ignore old responses
                    if (queryNumber != this.queryCount) {
                        return;
                    }

                    // ignore a response if the select2 has been closed before it was received
                    if (!this.opened()) {
                        this.search.removeClass("select2-active");
                        return;
                    }

                    // save context, if any
                    this.context = (data.context===undefined) ? null : data.context;
                    // create a default choice and prepend it to the list
                    if (this.opts.createSearchChoice && search.val() !== "") {
                        def = this.opts.createSearchChoice.call(self, search.val(), data.results);
                        if (def !== undefined && def !== null && self.id(def) !== undefined && self.id(def) !== null) {
                            if ($(data.results).filter(
                                function () {
                                    return equal(self.id(this), self.id(def));
                                }).length === 0) {
                                data.results.unshift(def);
                            }
                        }
                    }

                    if (data.results.length === 0 && checkFormatter(opts.formatNoMatches, "formatNoMatches")) {
                        render("<li class='select2-no-results'>" + opts.formatNoMatches(search.val()) + "</li>");
                        return;
                    }

                    results.empty();
                    self.opts.populateResults.call(this, results, data.results, {term: search.val(), page: this.resultsPage, context:null});

                    if (data.more === true && checkFormatter(opts.formatLoadMore, "formatLoadMore")) {
                        results.append("<li class='select2-more-results'>" + self.opts.escapeMarkup(opts.formatLoadMore(this.resultsPage)) + "</li>");
                        window.setTimeout(function() { self.loadMoreIfNeeded(); }, 10);
                    }

                    this.postprocessResults(data, initial);

                    postRender();

                    this.opts.element.trigger({ type: "select2-loaded", items: data });
                })});
        },

        // abstract
        cancel: function () {
            this.close();
        },

        // abstract
        blur: function () {
            // if selectOnBlur == true, select the currently highlighted option
            if (this.opts.selectOnBlur)
                this.selectHighlighted({noFocus: true});

            this.close();
            this.container.removeClass("select2-container-active");
            // synonymous to .is(':focus'), which is available in jquery >= 1.6
            if (this.search[0] === document.activeElement) { this.search.blur(); }
            this.clearSearch();
            this.selection.find(".select2-search-choice-focus").removeClass("select2-search-choice-focus");
        },

        // abstract
        focusSearch: function () {
            focus(this.search);
        },

        // abstract
        selectHighlighted: function (options) {
            var index=this.highlight(),
                highlighted=this.results.find(".select2-highlighted"),
                data = highlighted.closest('.select2-result').data("select2-data");

            if (data) {
                this.highlight(index);
                this.onSelect(data, options);
            } else if (options && options.noFocus) {
                this.close();
            }
        },

        // abstract
        getPlaceholder: function () {
            var placeholderOption;
            return this.opts.element.attr("placeholder") ||
                this.opts.element.attr("data-placeholder") || // jquery 1.4 compat
                this.opts.element.data("placeholder") ||
                this.opts.placeholder ||
                ((placeholderOption = this.getPlaceholderOption()) !== undefined ? placeholderOption.text() : undefined);
        },

        // abstract
        getPlaceholderOption: function() {
            if (this.select) {
                var firstOption = this.select.children('option').first();
                if (this.opts.placeholderOption !== undefined ) {
                    //Determine the placeholder option based on the specified placeholderOption setting
                    return (this.opts.placeholderOption === "first" && firstOption) ||
                        (typeof this.opts.placeholderOption === "function" && this.opts.placeholderOption(this.select));
                } else if (firstOption.text() === "" && firstOption.val() === "") {
                    //No explicit placeholder option specified, use the first if it's blank
                    return firstOption;
                }
            }
        },

        /**
         * Get the desired width for the container element.  This is
         * derived first from option `width` passed to select2, then
         * the inline 'style' on the original element, and finally
         * falls back to the jQuery calculated element width.
         */
        // abstract
        initContainerWidth: function () {
            function resolveContainerWidth() {
                var style, attrs, matches, i, l, attr;

                if (this.opts.width === "off") {
                    return null;
                } else if (this.opts.width === "element"){
                    return this.opts.element.outerWidth(false) === 0 ? 'auto' : this.opts.element.outerWidth(false) + 'px';
                } else if (this.opts.width === "copy" || this.opts.width === "resolve") {
                    // check if there is inline style on the element that contains width
                    style = this.opts.element.attr('style');
                    if (style !== undefined) {
                        attrs = style.split(';');
                        for (i = 0, l = attrs.length; i < l; i = i + 1) {
                            attr = attrs[i].replace(/\s/g, '');
                            matches = attr.match(/^width:(([-+]?([0-9]*\.)?[0-9]+)(px|em|ex|%|in|cm|mm|pt|pc))/i);
                            if (matches !== null && matches.length >= 1)
                                return matches[1];
                        }
                    }

                    if (this.opts.width === "resolve") {
                        // next check if css('width') can resolve a width that is percent based, this is sometimes possible
                        // when attached to input type=hidden or elements hidden via css
                        style = this.opts.element.css('width');
                        if (style.indexOf("%") > 0) return style;

                        // finally, fallback on the calculated width of the element
                        return (this.opts.element.outerWidth(false) === 0 ? 'auto' : this.opts.element.outerWidth(false) + 'px');
                    }

                    return null;
                } else if ($.isFunction(this.opts.width)) {
                    return this.opts.width();
                } else {
                    return this.opts.width;
                }
            };

            var width = resolveContainerWidth.call(this);
            if (width !== null) {
                this.container.css("width", width);
            }
        }
    });

    SingleSelect2 = clazz(AbstractSelect2, {

        // single

        createContainer: function () {
            var container = $(document.createElement("div")).attr({
                "class": "select2-container"
            }).html([
                    "<a href='javascript:void(0)' onclick='return false;' class='select2-choice' tabindex='-1'>",
                    "   <span class='select2-chosen'>&nbsp;</span><abbr class='select2-search-choice-close'></abbr>",
                    "   <span class='select2-arrow'><b></b></span>",
                    "</a>",
                    "<input class='select2-focusser select2-offscreen' type='text'/>",
                    "<div class='select2-drop select2-display-none'>",
                    "   <div class='select2-search'>",
                    "       <input type='text' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' class='select2-input'/>",
                    "   </div>",
                    "   <ul class='select2-results'>",
                    "   </ul>",
                    "</div>"].join(""));
            return container;
        },

        // single
        enableInterface: function() {
            if (this.parent.enableInterface.apply(this, arguments)) {
                this.focusser.prop("disabled", !this.isInterfaceEnabled());
            }
        },

        // single
        opening: function () {
            var el, range, len;

            if (this.opts.minimumResultsForSearch >= 0) {
                this.showSearch(true);
            }

            this.parent.opening.apply(this, arguments);

            if (this.showSearchInput !== false) {
                // IE appends focusser.val() at the end of field :/ so we manually insert it at the beginning using a range
                // all other browsers handle this just fine

                this.search.val(this.focusser.val());
            }
            this.search.focus();
            // move the cursor to the end after focussing, otherwise it will be at the beginning and
            // new text will appear *before* focusser.val()
            el = this.search.get(0);
            if (el.createTextRange) {
                range = el.createTextRange();
                range.collapse(false);
                range.select();
            } else if (el.setSelectionRange) {
                len = this.search.val().length;
                el.setSelectionRange(len, len);
            }

            // initializes search's value with nextSearchTerm (if defined by user)
            // ignore nextSearchTerm if the dropdown is opened by the user pressing a letter
            if(this.search.val() === "") {
                if(this.nextSearchTerm != undefined){
                    this.search.val(this.nextSearchTerm);
                    this.search.select();
                }
            }

            this.focusser.prop("disabled", true).val("");
            this.updateResults(true);
            this.opts.element.trigger($.Event("select2-open"));
        },

        // single
        close: function (params) {
            if (!this.opened()) return;
            this.parent.close.apply(this, arguments);

            params = params || {focus: true};
            this.focusser.removeAttr("disabled");

            if (params.focus) {
                this.focusser.focus();
            }
        },

        // single
        focus: function () {
            if (this.opened()) {
                this.close();
            } else {
                this.focusser.removeAttr("disabled");
                this.focusser.focus();
            }
        },

        // single
        isFocused: function () {
            return this.container.hasClass("select2-container-active");
        },

        // single
        cancel: function () {
            this.parent.cancel.apply(this, arguments);
            this.focusser.removeAttr("disabled");
            this.focusser.focus();
        },

        // single
        destroy: function() {
            $("label[for='" + this.focusser.attr('id') + "']")
                .attr('for', this.opts.element.attr("id"));
            this.parent.destroy.apply(this, arguments);
        },

        // single
        initContainer: function () {

            var selection,
                container = this.container,
                dropdown = this.dropdown;

            if (this.opts.minimumResultsForSearch < 0) {
                this.showSearch(false);
            } else {
                this.showSearch(true);
            }

            this.selection = selection = container.find(".select2-choice");

            this.focusser = container.find(".select2-focusser");

            // rewrite labels from original element to focusser
            this.focusser.attr("id", "s2id_autogen"+nextUid());

            $("label[for='" + this.opts.element.attr("id") + "']")
                .attr('for', this.focusser.attr('id'));

            this.focusser.attr("tabindex", this.elementTabIndex);

            this.search.on("keydown", this.bind(function (e) {
                if (!this.isInterfaceEnabled()) return;

                if (e.which === KEY.PAGE_UP || e.which === KEY.PAGE_DOWN) {
                    // prevent the page from scrolling
                    killEvent(e);
                    return;
                }

                switch (e.which) {
                    case KEY.UP:
                    case KEY.DOWN:
                        this.moveHighlight((e.which === KEY.UP) ? -1 : 1);
                        killEvent(e);
                        return;
                    case KEY.ENTER:
                        this.selectHighlighted();
                        killEvent(e);
                        return;
                    case KEY.TAB:
                        this.selectHighlighted({noFocus: true});
                        return;
                    case KEY.ESC:
                        this.cancel(e);
                        killEvent(e);
                        return;
                }
            }));

            this.search.on("blur", this.bind(function(e) {
                // a workaround for chrome to keep the search field focussed when the scroll bar is used to scroll the dropdown.
                // without this the search field loses focus which is annoying
                if (document.activeElement === this.body().get(0)) {
                    window.setTimeout(this.bind(function() {
                        this.search.focus();
                    }), 0);
                }
            }));

            this.focusser.on("keydown", this.bind(function (e) {
                if (!this.isInterfaceEnabled()) return;

                if (e.which === KEY.TAB || KEY.isControl(e) || KEY.isFunctionKey(e) || e.which === KEY.ESC) {
                    return;
                }

                if (this.opts.openOnEnter === false && e.which === KEY.ENTER) {
                    killEvent(e);
                    return;
                }

                if (e.which == KEY.DOWN || e.which == KEY.UP
                    || (e.which == KEY.ENTER && this.opts.openOnEnter)) {

                    if (e.altKey || e.ctrlKey || e.shiftKey || e.metaKey) return;

                    this.open();
                    killEvent(e);
                    return;
                }

                if (e.which == KEY.DELETE || e.which == KEY.BACKSPACE) {
                    if (this.opts.allowClear) {
                        this.clear();
                    }
                    killEvent(e);
                    return;
                }
            }));


            installKeyUpChangeEvent(this.focusser);
            this.focusser.on("keyup-change input", this.bind(function(e) {
                if (this.opts.minimumResultsForSearch >= 0) {
                    e.stopPropagation();
                    if (this.opened()) return;
                    this.open();
                }
            }));

            selection.on("mousedown", "abbr", this.bind(function (e) {
                if (!this.isInterfaceEnabled()) return;
                this.clear();
                killEventImmediately(e);
                this.close();
                this.selection.focus();
            }));

            selection.on("mousedown", this.bind(function (e) {

                if (!this.container.hasClass("select2-container-active")) {
                    this.opts.element.trigger($.Event("select2-focus"));
                }

                if (this.opened()) {
                    this.close();
                } else if (this.isInterfaceEnabled()) {
                    this.open();
                }

                killEvent(e);
            }));

            dropdown.on("mousedown", this.bind(function() { this.search.focus(); }));

            selection.on("focus", this.bind(function(e) {
                killEvent(e);
            }));

            this.focusser.on("focus", this.bind(function(){
                    if (!this.container.hasClass("select2-container-active")) {
                        this.opts.element.trigger($.Event("select2-focus"));
                    }
                    this.container.addClass("select2-container-active");
                })).on("blur", this.bind(function() {
                    if (!this.opened()) {
                        this.container.removeClass("select2-container-active");
                        this.opts.element.trigger($.Event("select2-blur"));
                    }
                }));
            this.search.on("focus", this.bind(function(){
                if (!this.container.hasClass("select2-container-active")) {
                    this.opts.element.trigger($.Event("select2-focus"));
                }
                this.container.addClass("select2-container-active");
            }));

            this.initContainerWidth();
            this.opts.element.addClass("select2-offscreen");
            this.setPlaceholder();

        },

        // single
        clear: function(triggerChange) {
            var data=this.selection.data("select2-data");
            if (data) { // guard against queued quick consecutive clicks
                var evt = $.Event("select2-clearing");
                this.opts.element.trigger(evt);
                if (evt.isDefaultPrevented()) {
                    return;
                }
                var placeholderOption = this.getPlaceholderOption();
                this.opts.element.val(placeholderOption ? placeholderOption.val() : "");
                this.selection.find(".select2-chosen").empty();
                this.selection.removeData("select2-data");
                this.setPlaceholder();

                if (triggerChange !== false){
                    this.opts.element.trigger({ type: "select2-removed", val: this.id(data), choice: data });
                    this.triggerChange({removed:data});
                }
            }
        },

        /**
         * Sets selection based on source element's value
         */
        // single
        initSelection: function () {
            var selected;
            if (this.isPlaceholderOptionSelected()) {
                this.updateSelection(null);
                this.close();
                this.setPlaceholder();
            } else {
                var self = this;
                this.opts.initSelection.call(null, this.opts.element, function(selected){
                    if (selected !== undefined && selected !== null) {
                        self.updateSelection(selected);
                        self.close();
                        self.setPlaceholder();
                    }
                });
            }
        },

        isPlaceholderOptionSelected: function() {
            var placeholderOption;
            if (!this.getPlaceholder()) return false; // no placeholder specified so no option should be considered
            return ((placeholderOption = this.getPlaceholderOption()) !== undefined && placeholderOption.prop("selected"))
                || (this.opts.element.val() === "")
                || (this.opts.element.val() === undefined)
                || (this.opts.element.val() === null);
        },

        // single
        prepareOpts: function () {
            var opts = this.parent.prepareOpts.apply(this, arguments),
                self=this;

            if (opts.element.get(0).tagName.toLowerCase() === "select") {
                // install the selection initializer
                opts.initSelection = function (element, callback) {
                    var selected = element.find("option").filter(function() { return this.selected });
                    // a single select box always has a value, no need to null check 'selected'
                    callback(self.optionToData(selected));
                };
            } else if ("data" in opts) {
                // install default initSelection when applied to hidden input and data is local
                opts.initSelection = opts.initSelection || function (element, callback) {
                    var id = element.val();
                    //search in data by id, storing the actual matching item
                    var match = null;
                    opts.query({
                        matcher: function(term, text, el){
                            var is_match = equal(id, opts.id(el));
                            if (is_match) {
                                match = el;
                            }
                            return is_match;
                        },
                        callback: !$.isFunction(callback) ? $.noop : function() {
                            callback(match);
                        }
                    });
                };
            }

            return opts;
        },

        // single
        getPlaceholder: function() {
            // if a placeholder is specified on a single select without a valid placeholder option ignore it
            if (this.select) {
                if (this.getPlaceholderOption() === undefined) {
                    return undefined;
                }
            }

            return this.parent.getPlaceholder.apply(this, arguments);
        },

        // single
        setPlaceholder: function () {
            var placeholder = this.getPlaceholder();

            if (this.isPlaceholderOptionSelected() && placeholder !== undefined) {

                // check for a placeholder option if attached to a select
                if (this.select && this.getPlaceholderOption() === undefined) return;

                this.selection.find(".select2-chosen").html(this.opts.escapeMarkup(placeholder));

                this.selection.addClass("select2-default");

                this.container.removeClass("select2-allowclear");
            }
        },

        // single
        postprocessResults: function (data, initial, noHighlightUpdate) {
            var selected = 0, self = this, showSearchInput = true;

            // find the selected element in the result list

            this.findHighlightableChoices().each2(function (i, elm) {
                if (equal(self.id(elm.data("select2-data")), self.opts.element.val())) {
                    selected = i;
                    return false;
                }
            });

            // and highlight it
            if (noHighlightUpdate !== false) {
                if (initial === true && selected >= 0) {
                    this.highlight(selected);
                } else {
                    this.highlight(0);
                }
            }

            // hide the search box if this is the first we got the results and there are enough of them for search

            if (initial === true) {
                var min = this.opts.minimumResultsForSearch;
                if (min >= 0) {
                    this.showSearch(countResults(data.results) >= min);
                }
            }
        },

        // single
        showSearch: function(showSearchInput) {
            if (this.showSearchInput === showSearchInput) return;

            this.showSearchInput = showSearchInput;

            this.dropdown.find(".select2-search").toggleClass("select2-search-hidden", !showSearchInput);
            this.dropdown.find(".select2-search").toggleClass("select2-offscreen", !showSearchInput);
            //add "select2-with-searchbox" to the container if search box is shown
            $(this.dropdown, this.container).toggleClass("select2-with-searchbox", showSearchInput);
        },

        // single
        onSelect: function (data, options) {

            if (!this.triggerSelect(data)) { return; }

            var old = this.opts.element.val(),
                oldData = this.data();

            this.opts.element.val(this.id(data));
            this.updateSelection(data);

            this.opts.element.trigger({ type: "select2-selected", val: this.id(data), choice: data });

            this.nextSearchTerm = this.opts.nextSearchTerm(data, this.search.val());
            this.close();

            if (!options || !options.noFocus)
                this.focusser.focus();

            if (!equal(old, this.id(data))) { this.triggerChange({added:data,removed:oldData}); }
        },

        // single
        updateSelection: function (data) {

            var container=this.selection.find(".select2-chosen"), formatted, cssClass;

            this.selection.data("select2-data", data);

            container.empty();
            if (data !== null) {
                formatted=this.opts.formatSelection(data, container, this.opts.escapeMarkup);
            }
            if (formatted !== undefined) {
                container.append(formatted);
            }
            cssClass=this.opts.formatSelectionCssClass(data, container);
            if (cssClass !== undefined) {
                container.addClass(cssClass);
            }

            this.selection.removeClass("select2-default");

            if (this.opts.allowClear && this.getPlaceholder() !== undefined) {
                this.container.addClass("select2-allowclear");
            }
        },

        // single
        val: function () {
            var val,
                triggerChange = false,
                data = null,
                self = this,
                oldData = this.data();

            if (arguments.length === 0) {
                return this.opts.element.val();
            }

            val = arguments[0];

            if (arguments.length > 1) {
                triggerChange = arguments[1];
            }

            if (this.select) {
                this.select
                    .val(val)
                    .find("option").filter(function() { return this.selected }).each2(function (i, elm) {
                        data = self.optionToData(elm);
                        return false;
                    });
                this.updateSelection(data);
                this.setPlaceholder();
                if (triggerChange) {
                    this.triggerChange({added: data, removed:oldData});
                }
            } else {
                // val is an id. !val is true for [undefined,null,'',0] - 0 is legal
                if (!val && val !== 0) {
                    this.clear(triggerChange);
                    return;
                }
                if (this.opts.initSelection === undefined) {
                    throw new Error("cannot call val() if initSelection() is not defined");
                }
                this.opts.element.val(val);
                this.opts.initSelection(this.opts.element, function(data){
                    self.opts.element.val(!data ? "" : self.id(data));
                    self.updateSelection(data);
                    self.setPlaceholder();
                    if (triggerChange) {
                        self.triggerChange({added: data, removed:oldData});
                    }
                });
            }
        },

        // single
        clearSearch: function () {
            this.search.val("");
            this.focusser.val("");
        },

        // single
        data: function(value) {
            var data,
                triggerChange = false;

            if (arguments.length === 0) {
                data = this.selection.data("select2-data");
                if (data == undefined) data = null;
                return data;
            } else {
                if (arguments.length > 1) {
                    triggerChange = arguments[1];
                }
                if (!value) {
                    this.clear(triggerChange);
                } else {
                    data = this.data();
                    this.opts.element.val(!value ? "" : this.id(value));
                    this.updateSelection(value);
                    if (triggerChange) {
                        this.triggerChange({added: value, removed:data});
                    }
                }
            }
        }
    });

    MultiSelect2 = clazz(AbstractSelect2, {

        // multi
        createContainer: function () {
            var container = $(document.createElement("div")).attr({
                "class": "select2-container select2-container-multi"
            }).html([
                    "<ul class='select2-choices'>",
                    "  <li class='select2-search-field'>",
                    "    <input type='text' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' class='select2-input'>",
                    "  </li>",
                    "</ul>",
                    "<div class='select2-drop select2-drop-multi select2-display-none'>",
                    "   <ul class='select2-results'>",
                    "   </ul>",
                    "</div>"].join(""));
            return container;
        },

        // multi
        prepareOpts: function () {
            var opts = this.parent.prepareOpts.apply(this, arguments),
                self=this;

            // TODO validate placeholder is a string if specified

            if (opts.element.get(0).tagName.toLowerCase() === "select") {
                // install sthe selection initializer
                opts.initSelection = function (element, callback) {

                    var data = [];

                    element.find("option").filter(function() { return this.selected }).each2(function (i, elm) {
                        data.push(self.optionToData(elm));
                    });
                    callback(data);
                };
            } else if ("data" in opts) {
                // install default initSelection when applied to hidden input and data is local
                opts.initSelection = opts.initSelection || function (element, callback) {
                    var ids = splitVal(element.val(), opts.separator);
                    //search in data by array of ids, storing matching items in a list
                    var matches = [];
                    opts.query({
                        matcher: function(term, text, el){
                            var is_match = $.grep(ids, function(id) {
                                return equal(id, opts.id(el));
                            }).length;
                            if (is_match) {
                                matches.push(el);
                            }
                            return is_match;
                        },
                        callback: !$.isFunction(callback) ? $.noop : function() {
                            // reorder matches based on the order they appear in the ids array because right now
                            // they are in the order in which they appear in data array
                            var ordered = [];
                            for (var i = 0; i < ids.length; i++) {
                                var id = ids[i];
                                for (var j = 0; j < matches.length; j++) {
                                    var match = matches[j];
                                    if (equal(id, opts.id(match))) {
                                        ordered.push(match);
                                        matches.splice(j, 1);
                                        break;
                                    }
                                }
                            }
                            callback(ordered);
                        }
                    });
                };
            }

            return opts;
        },

        // multi
        selectChoice: function (choice) {

            var selected = this.container.find(".select2-search-choice-focus");
            if (selected.length && choice && choice[0] == selected[0]) {

            } else {
                if (selected.length) {
                    this.opts.element.trigger("choice-deselected", selected);
                }
                selected.removeClass("select2-search-choice-focus");
                if (choice && choice.length) {
                    this.close();
                    choice.addClass("select2-search-choice-focus");
                    this.opts.element.trigger("choice-selected", choice);
                }
            }
        },

        // multi
        destroy: function() {
            $("label[for='" + this.search.attr('id') + "']")
                .attr('for', this.opts.element.attr("id"));
            this.parent.destroy.apply(this, arguments);
        },

        // multi
        initContainer: function () {

            var selector = ".select2-choices", selection;

            this.searchContainer = this.container.find(".select2-search-field");
            this.selection = selection = this.container.find(selector);

            var _this = this;
            this.selection.on("click", ".select2-search-choice:not(.select2-locked)", function (e) {
                //killEvent(e);
                _this.search[0].focus();
                _this.selectChoice($(this));
            });

            // rewrite labels from original element to focusser
            this.search.attr("id", "s2id_autogen"+nextUid());
            $("label[for='" + this.opts.element.attr("id") + "']")
                .attr('for', this.search.attr('id'));

            this.search.on("input paste", this.bind(function() {
                if (!this.isInterfaceEnabled()) return;
                if (!this.opened()) {
                    this.open();
                }
            }));

            this.search.attr("tabindex", this.elementTabIndex);

            this.keydowns = 0;
            this.search.on("keydown", this.bind(function (e) {
                if (!this.isInterfaceEnabled()) return;

                ++this.keydowns;
                var selected = selection.find(".select2-search-choice-focus");
                var prev = selected.prev(".select2-search-choice:not(.select2-locked)");
                var next = selected.next(".select2-search-choice:not(.select2-locked)");
                var pos = getCursorInfo(this.search);

                if (selected.length &&
                    (e.which == KEY.LEFT || e.which == KEY.RIGHT || e.which == KEY.BACKSPACE || e.which == KEY.DELETE || e.which == KEY.ENTER)) {
                    var selectedChoice = selected;
                    if (e.which == KEY.LEFT && prev.length) {
                        selectedChoice = prev;
                    }
                    else if (e.which == KEY.RIGHT) {
                        selectedChoice = next.length ? next : null;
                    }
                    else if (e.which === KEY.BACKSPACE) {
                        this.unselect(selected.first());
                        this.search.width(10);
                        selectedChoice = prev.length ? prev : next;
                    } else if (e.which == KEY.DELETE) {
                        this.unselect(selected.first());
                        this.search.width(10);
                        selectedChoice = next.length ? next : null;
                    } else if (e.which == KEY.ENTER) {
                        selectedChoice = null;
                    }

                    this.selectChoice(selectedChoice);
                    killEvent(e);
                    if (!selectedChoice || !selectedChoice.length) {
                        this.open();
                    }
                    return;
                } else if (((e.which === KEY.BACKSPACE && this.keydowns == 1)
                    || e.which == KEY.LEFT) && (pos.offset == 0 && !pos.length)) {

                    this.selectChoice(selection.find(".select2-search-choice:not(.select2-locked)").last());
                    killEvent(e);
                    return;
                } else {
                    this.selectChoice(null);
                }

                if (this.opened()) {
                    switch (e.which) {
                        case KEY.UP:
                        case KEY.DOWN:
                            this.moveHighlight((e.which === KEY.UP) ? -1 : 1);
                            killEvent(e);
                            return;
                        case KEY.ENTER:
                            this.selectHighlighted();
                            killEvent(e);
                            return;
                        case KEY.TAB:
                            this.selectHighlighted({noFocus:true});
                            this.close();
                            return;
                        case KEY.ESC:
                            this.cancel(e);
                            killEvent(e);
                            return;
                    }
                }

                if (e.which === KEY.TAB || KEY.isControl(e) || KEY.isFunctionKey(e)
                    || e.which === KEY.BACKSPACE || e.which === KEY.ESC) {
                    return;
                }

                if (e.which === KEY.ENTER) {
                    if (this.opts.openOnEnter === false) {
                        return;
                    } else if (e.altKey || e.ctrlKey || e.shiftKey || e.metaKey) {
                        return;
                    }
                }

                this.open();

                if (e.which === KEY.PAGE_UP || e.which === KEY.PAGE_DOWN) {
                    // prevent the page from scrolling
                    killEvent(e);
                }

                if (e.which === KEY.ENTER) {
                    // prevent form from being submitted
                    killEvent(e);
                }

            }));

            this.search.on("keyup", this.bind(function (e) {
                this.keydowns = 0;
                this.resizeSearch();
            })
            );

            this.search.on("blur", this.bind(function(e) {
                this.container.removeClass("select2-container-active");
                this.search.removeClass("select2-focused");
                this.selectChoice(null);
                if (!this.opened()) this.clearSearch();
                e.stopImmediatePropagation();
                this.opts.element.trigger($.Event("select2-blur"));
            }));

            this.container.on("click", selector, this.bind(function (e) {
                if (!this.isInterfaceEnabled()) return;
                if ($(e.target).closest(".select2-search-choice").length > 0) {
                    // clicked inside a select2 search choice, do not open
                    return;
                }
                this.selectChoice(null);
                this.clearPlaceholder();
                if (!this.container.hasClass("select2-container-active")) {
                    this.opts.element.trigger($.Event("select2-focus"));
                }
                this.open();
                this.focusSearch();
                e.preventDefault();
            }));

            this.container.on("focus", selector, this.bind(function () {
                if (!this.isInterfaceEnabled()) return;
                if (!this.container.hasClass("select2-container-active")) {
                    this.opts.element.trigger($.Event("select2-focus"));
                }
                this.container.addClass("select2-container-active");
                this.dropdown.addClass("select2-drop-active");
                this.clearPlaceholder();
            }));

            this.initContainerWidth();
            this.opts.element.addClass("select2-offscreen");

            // set the placeholder if necessary
            this.clearSearch();
        },

        // multi
        enableInterface: function() {
            if (this.parent.enableInterface.apply(this, arguments)) {
                this.search.prop("disabled", !this.isInterfaceEnabled());
            }
        },

        // multi
        initSelection: function () {
            var data;
            if (this.opts.element.val() === "" && this.opts.element.text() === "") {
                this.updateSelection([]);
                this.close();
                // set the placeholder if necessary
                this.clearSearch();
            }
            if (this.select || this.opts.element.val() !== "") {
                var self = this;
                this.opts.initSelection.call(null, this.opts.element, function(data){
                    if (data !== undefined && data !== null) {
                        self.updateSelection(data);
                        self.close();
                        // set the placeholder if necessary
                        self.clearSearch();
                    }
                });
            }
        },

        // multi
        clearSearch: function () {
            var placeholder = this.getPlaceholder(),
                maxWidth = this.getMaxSearchWidth();

            if (placeholder !== undefined  && this.getVal().length === 0 && this.search.hasClass("select2-focused") === false) {
                this.search.val(placeholder).addClass("select2-default");
                // stretch the search box to full width of the container so as much of the placeholder is visible as possible
                // we could call this.resizeSearch(), but we do not because that requires a sizer and we do not want to create one so early because of a firefox bug, see #944
                this.search.width(maxWidth > 0 ? maxWidth : this.container.css("width"));
            } else {
                this.search.val("").width(10);
            }
        },

        // multi
        clearPlaceholder: function () {
            if (this.search.hasClass("select2-default")) {
                this.search.val("").removeClass("select2-default");
            }
        },

        // multi
        opening: function () {
            this.clearPlaceholder(); // should be done before super so placeholder is not used to search
            this.resizeSearch();

            this.parent.opening.apply(this, arguments);

            this.focusSearch();

            this.updateResults(true);
            this.search.focus();
            this.opts.element.trigger($.Event("select2-open"));
        },

        // multi
        close: function () {
            if (!this.opened()) return;
            this.parent.close.apply(this, arguments);
        },

        // multi
        focus: function () {
            this.close();
            this.search.focus();
        },

        // multi
        isFocused: function () {
            return this.search.hasClass("select2-focused");
        },

        // multi
        updateSelection: function (data) {
            var ids = [], filtered = [], self = this;

            // filter out duplicates
            $(data).each(function () {
                if (indexOf(self.id(this), ids) < 0) {
                    ids.push(self.id(this));
                    filtered.push(this);
                }
            });
            data = filtered;

            this.selection.find(".select2-search-choice").remove();
            $(data).each(function () {
                self.addSelectedChoice(this);
            });
            self.postprocessResults();
        },

        // multi
        tokenize: function() {
            var input = this.search.val();
            input = this.opts.tokenizer.call(this, input, this.data(), this.bind(this.onSelect), this.opts);
            if (input != null && input != undefined) {
                this.search.val(input);
                if (input.length > 0) {
                    this.open();
                }
            }

        },

        // multi
        onSelect: function (data, options) {

            if (!this.triggerSelect(data)) { return; }

            this.addSelectedChoice(data);

            this.opts.element.trigger({ type: "selected", val: this.id(data), choice: data });

            if (this.select || !this.opts.closeOnSelect) this.postprocessResults(data, false, this.opts.closeOnSelect===true);

            if (this.opts.closeOnSelect) {
                this.close();
                this.search.width(10);
            } else {
                if (this.countSelectableResults()>0) {
                    this.search.width(10);
                    this.resizeSearch();
                    if (this.getMaximumSelectionSize() > 0 && this.val().length >= this.getMaximumSelectionSize()) {
                        // if we reached max selection size repaint the results so choices
                        // are replaced with the max selection reached message
                        this.updateResults(true);
                    }
                    this.positionDropdown();
                } else {
                    // if nothing left to select close
                    this.close();
                    this.search.width(10);
                }
            }

            // since its not possible to select an element that has already been
            // added we do not need to check if this is a new element before firing change
            this.triggerChange({ added: data });

            if (!options || !options.noFocus)
                this.focusSearch();
        },

        // multi
        cancel: function () {
            this.close();
            this.focusSearch();
        },

        addSelectedChoice: function (data) {
            var enableChoice = !data.locked,
                enabledItem = $(
                    "<li class='select2-search-choice'>" +
                        "    <div></div>" +
                        "    <a href='#' onclick='return false;' class='select2-search-choice-close' tabindex='-1'></a>" +
                        "</li>"),
                disabledItem = $(
                    "<li class='select2-search-choice select2-locked'>" +
                        "<div></div>" +
                        "</li>");
            var choice = enableChoice ? enabledItem : disabledItem,
                id = this.id(data),
                val = this.getVal(),
                formatted,
                cssClass;

            formatted=this.opts.formatSelection(data, choice.find("div"), this.opts.escapeMarkup);
            if (formatted != undefined) {
                choice.find("div").replaceWith("<div>"+formatted+"</div>");
            }
            cssClass=this.opts.formatSelectionCssClass(data, choice.find("div"));
            if (cssClass != undefined) {
                choice.addClass(cssClass);
            }

            if(enableChoice){
                choice.find(".select2-search-choice-close")
                    .on("mousedown", killEvent)
                    .on("click dblclick", this.bind(function (e) {
                        if (!this.isInterfaceEnabled()) return;

                        $(e.target).closest(".select2-search-choice").fadeOut('fast', this.bind(function(){
                            this.unselect($(e.target));
                            this.selection.find(".select2-search-choice-focus").removeClass("select2-search-choice-focus");
                            this.close();
                            this.focusSearch();
                        })).dequeue();
                        killEvent(e);
                    })).on("focus", this.bind(function () {
                        if (!this.isInterfaceEnabled()) return;
                        this.container.addClass("select2-container-active");
                        this.dropdown.addClass("select2-drop-active");
                    }));
            }

            choice.data("select2-data", data);
            choice.insertBefore(this.searchContainer);

            val.push(id);
            this.setVal(val);
        },

        // multi
        unselect: function (selected) {
            var val = this.getVal(),
                data,
                index;
            selected = selected.closest(".select2-search-choice");

            if (selected.length === 0) {
                throw "Invalid argument: " + selected + ". Must be .select2-search-choice";
            }

            data = selected.data("select2-data");

            if (!data) {
                // prevent a race condition when the 'x' is clicked really fast repeatedly the event can be queued
                // and invoked on an element already removed
                return;
            }

            while((index = indexOf(this.id(data), val)) >= 0) {
                val.splice(index, 1);
                this.setVal(val);
                if (this.select) this.postprocessResults();
            }

            var evt = $.Event("select2-removing");
            evt.val = this.id(data);
            evt.choice = data;
            this.opts.element.trigger(evt);

            if (evt.isDefaultPrevented()) {
                return;
            }

            selected.remove();

            this.opts.element.trigger({ type: "select2-removed", val: this.id(data), choice: data });
            this.triggerChange({ removed: data });
        },

        // multi
        postprocessResults: function (data, initial, noHighlightUpdate) {
            var val = this.getVal(),
                choices = this.results.find(".select2-result"),
                compound = this.results.find(".select2-result-with-children"),
                self = this;

            choices.each2(function (i, choice) {
                var id = self.id(choice.data("select2-data"));
                if (indexOf(id, val) >= 0) {
                    choice.addClass("select2-selected");
                    // mark all children of the selected parent as selected
                    choice.find(".select2-result-selectable").addClass("select2-selected");
                }
            });

            compound.each2(function(i, choice) {
                // hide an optgroup if it doesnt have any selectable children
                if (!choice.is('.select2-result-selectable')
                    && choice.find(".select2-result-selectable:not(.select2-selected)").length === 0) {
                    choice.addClass("select2-selected");
                }
            });

            if (this.highlight() == -1 && noHighlightUpdate !== false){
                self.highlight(0);
            }

            //If all results are chosen render formatNoMAtches
            if(!this.opts.createSearchChoice && !choices.filter('.select2-result:not(.select2-selected)').length > 0){
                if(!data || data && !data.more && this.results.find(".select2-no-results").length === 0) {
                    if (checkFormatter(self.opts.formatNoMatches, "formatNoMatches")) {
                        this.results.append("<li class='select2-no-results'>" + self.opts.formatNoMatches(self.search.val()) + "</li>");
                    }
                }
            }

        },

        // multi
        getMaxSearchWidth: function() {
            return this.selection.width() - getSideBorderPadding(this.search);
        },

        // multi
        resizeSearch: function () {
            var minimumWidth, left, maxWidth, containerLeft, searchWidth,
                sideBorderPadding = getSideBorderPadding(this.search);

            minimumWidth = measureTextWidth(this.search) + 10;

            left = this.search.offset().left;

            maxWidth = this.selection.width();
            containerLeft = this.selection.offset().left;

            searchWidth = maxWidth - (left - containerLeft) - sideBorderPadding;

            if (searchWidth < minimumWidth) {
                searchWidth = maxWidth - sideBorderPadding;
            }

            if (searchWidth < 40) {
                searchWidth = maxWidth - sideBorderPadding;
            }

            if (searchWidth <= 0) {
                searchWidth = minimumWidth;
            }

            this.search.width(Math.floor(searchWidth));
        },

        // multi
        getVal: function () {
            var val;
            if (this.select) {
                val = this.select.val();
                return val === null ? [] : val;
            } else {
                val = this.opts.element.val();
                return splitVal(val, this.opts.separator);
            }
        },

        // multi
        setVal: function (val) {
            var unique;
            if (this.select) {
                this.select.val(val);
            } else {
                unique = [];
                // filter out duplicates
                $(val).each(function () {
                    if (indexOf(this, unique) < 0) unique.push(this);
                });
                this.opts.element.val(unique.length === 0 ? "" : unique.join(this.opts.separator));
            }
        },

        // multi
        buildChangeDetails: function (old, current) {
            var current = current.slice(0),
                old = old.slice(0);

            // remove intersection from each array
            for (var i = 0; i < current.length; i++) {
                for (var j = 0; j < old.length; j++) {
                    if (equal(this.opts.id(current[i]), this.opts.id(old[j]))) {
                        current.splice(i, 1);
                        if(i>0){
                            i--;
                        }
                        old.splice(j, 1);
                        j--;
                    }
                }
            }

            return {added: current, removed: old};
        },


        // multi
        val: function (val, triggerChange) {
            var oldData, self=this;

            if (arguments.length === 0) {
                return this.getVal();
            }

            oldData=this.data();
            if (!oldData.length) oldData=[];

            // val is an id. !val is true for [undefined,null,'',0] - 0 is legal
            if (!val && val !== 0) {
                this.opts.element.val("");
                this.updateSelection([]);
                this.clearSearch();
                if (triggerChange) {
                    this.triggerChange({added: this.data(), removed: oldData});
                }
                return;
            }

            // val is a list of ids
            this.setVal(val);

            if (this.select) {
                this.opts.initSelection(this.select, this.bind(this.updateSelection));
                if (triggerChange) {
                    this.triggerChange(this.buildChangeDetails(oldData, this.data()));
                }
            } else {
                if (this.opts.initSelection === undefined) {
                    throw new Error("val() cannot be called if initSelection() is not defined");
                }

                this.opts.initSelection(this.opts.element, function(data){
                    var ids=$.map(data, self.id);
                    self.setVal(ids);
                    self.updateSelection(data);
                    self.clearSearch();
                    if (triggerChange) {
                        self.triggerChange(self.buildChangeDetails(oldData, self.data()));
                    }
                });
            }
            this.clearSearch();
        },

        // multi
        onSortStart: function() {
            if (this.select) {
                throw new Error("Sorting of elements is not supported when attached to <select>. Attach to <input type='hidden'/> instead.");
            }

            // collapse search field into 0 width so its container can be collapsed as well
            this.search.width(0);
            // hide the container
            this.searchContainer.hide();
        },

        // multi
        onSortEnd:function() {

            var val=[], self=this;

            // show search and move it to the end of the list
            this.searchContainer.show();
            // make sure the search container is the last item in the list
            this.searchContainer.appendTo(this.searchContainer.parent());
            // since we collapsed the width in dragStarted, we resize it here
            this.resizeSearch();

            // update selection
            this.selection.find(".select2-search-choice").each(function() {
                val.push(self.opts.id($(this).data("select2-data")));
            });
            this.setVal(val);
            this.triggerChange();
        },

        // multi
        data: function(values, triggerChange) {
            var self=this, ids, old;
            if (arguments.length === 0) {
                return this.selection
                    .find(".select2-search-choice")
                    .map(function() { return $(this).data("select2-data"); })
                    .get();
            } else {
                old = this.data();
                if (!values) { values = []; }
                ids = $.map(values, function(e) { return self.opts.id(e); });
                this.setVal(ids);
                this.updateSelection(values);
                this.clearSearch();
                if (triggerChange) {
                    this.triggerChange(this.buildChangeDetails(old, this.data()));
                }
            }
        }
    });

    $.fn.select2 = function () {

        var args = Array.prototype.slice.call(arguments, 0),
            opts,
            select2,
            method, value, multiple,
            allowedMethods = ["val", "destroy", "opened", "open", "close", "focus", "isFocused", "container", "dropdown", "onSortStart", "onSortEnd", "enable", "disable", "readonly", "positionDropdown", "data", "search"],
            valueMethods = ["opened", "isFocused", "container", "dropdown"],
            propertyMethods = ["val", "data"],
            methodsMap = { search: "externalSearch" };

        this.each(function () {
            if (args.length === 0 || typeof(args[0]) === "object") {
                opts = args.length === 0 ? {} : $.extend({}, args[0]);
                opts.element = $(this);

                if (opts.element.get(0).tagName.toLowerCase() === "select") {
                    multiple = opts.element.prop("multiple");
                } else {
                    multiple = opts.multiple || false;
                    if ("tags" in opts) {opts.multiple = multiple = true;}
                }

                select2 = multiple ? new MultiSelect2() : new SingleSelect2();
                select2.init(opts);
            } else if (typeof(args[0]) === "string") {

                if (indexOf(args[0], allowedMethods) < 0) {
                    throw "Unknown method: " + args[0];
                }

                value = undefined;
                select2 = $(this).data("select2");
                if (select2 === undefined) return;

                method=args[0];

                if (method === "container") {
                    value = select2.container;
                } else if (method === "dropdown") {
                    value = select2.dropdown;
                } else {
                    if (methodsMap[method]) method = methodsMap[method];

                    value = select2[method].apply(select2, args.slice(1));
                }
                if (indexOf(args[0], valueMethods) >= 0
                    || (indexOf(args[0], propertyMethods) && args.length == 1)) {
                    return false; // abort the iteration, ready to return first matched value
                }
            } else {
                throw "Invalid arguments to select2 plugin: " + args;
            }
        });
        return (value === undefined) ? this : value;
    };

    // plugin defaults, accessible to users
    $.fn.select2.defaults = {
        width: "copy",
        loadMorePadding: 0,
        closeOnSelect: true,
        openOnEnter: true,
        containerCss: {},
        dropdownCss: {},
        containerCssClass: "",
        dropdownCssClass: "",
        formatResult: function(result, container, query, escapeMarkup) {
            var markup=[];
            markMatch(result.text, query.term, markup, escapeMarkup);
            return markup.join("");
        },
        formatSelection: function (data, container, escapeMarkup) {
            return data ? escapeMarkup(data.text) : undefined;
        },
        sortResults: function (results, container, query) {
            return results;
        },
        formatResultCssClass: function(data) {return undefined;},
        formatSelectionCssClass: function(data, container) {return undefined;},
        formatNoMatches: function () { return "No matches found"; },
        formatInputTooShort: function (input, min) { var n = min - input.length; return "Please enter " + n + " more character" + (n == 1? "" : "s"); },
        formatInputTooLong: function (input, max) { var n = input.length - max; return "Please delete " + n + " character" + (n == 1? "" : "s"); },
        formatSelectionTooBig: function (limit) { return "You can only select " + limit + " item" + (limit == 1 ? "" : "s"); },
        formatLoadMore: function (pageNumber) { return "Loading more results..."; },
        formatSearching: function () { return "Searching..."; },
        minimumResultsForSearch: 0,
        minimumInputLength: 0,
        maximumInputLength: null,
        maximumSelectionSize: 0,
        id: function (e) { return e.id; },
        matcher: function(term, text) {
            return stripDiacritics(''+text).toUpperCase().indexOf(stripDiacritics(''+term).toUpperCase()) >= 0;
        },
        separator: ",",
        tokenSeparators: [],
        tokenizer: defaultTokenizer,
        escapeMarkup: defaultEscapeMarkup,
        blurOnChange: false,
        selectOnBlur: false,
        adaptContainerCssClass: function(c) { return c; },
        adaptDropdownCssClass: function(c) { return null; },
        nextSearchTerm: function(selectedObject, currentSearchTerm) { return undefined; }
    };

    $.fn.select2.ajaxDefaults = {
        transport: $.ajax,
        params: {
            type: "GET",
            cache: false,
            dataType: "json"
        }
    };

    // exports
    window.Select2 = {
        query: {
            ajax: ajax,
            local: local,
            tags: tags
        }, util: {
            debounce: debounce,
            markMatch: markMatch,
            escapeMarkup: defaultEscapeMarkup,
            stripDiacritics: stripDiacritics
        }, "class": {
            "abstract": AbstractSelect2,
            "single": SingleSelect2,
            "multi": MultiSelect2
        }
    };

}(jQuery));

/**
 * Wraps a vanilla Select2 with ADG _style_, as an auiSelect2 method on jQuery objects.
 *
 * @since 5.2
 */
(function($) {

    /**
     * We make a copy of the original select2 so that later we might re-specify $.fn.auiSelect2 as $.fn.select2. That
     * way, calling code will be able to call $thing.select2() as if they were calling the original library,
     * and ADG styling will just magically happen.
     */
    var originalSelect2 = $.fn.select2;

    // AUI-specific classes
    var auiContainer = "aui-select2-container";
    var auiDropdown = "aui-select2-drop aui-dropdown2 aui-style-default";
    var auiHasAvatar = "aui-has-avatar";

    $.fn.auiSelect2 = function (first) {
        var updatedArgs;

        if($.isPlainObject(first)) {
            var auiOpts = $.extend({}, first);
            var auiAvatarClass = auiOpts.hasAvatar ? " " + auiHasAvatar : "";
            //add our classes in addition to those the caller specified
            auiOpts.containerCssClass  = auiContainer + auiAvatarClass + (auiOpts.containerCssClass ? " " + auiOpts.containerCssClass : "");
            auiOpts.dropdownCssClass  = auiDropdown + auiAvatarClass + (auiOpts.dropdownCssClass  ? " " + auiOpts.dropdownCssClass : "");
            updatedArgs = Array.prototype.slice.call(arguments, 1);
            updatedArgs.unshift(auiOpts);
        } else if (!arguments.length) {
            updatedArgs = [{
                containerCssClass: auiContainer,
                dropdownCssClass: auiDropdown
            }];
        } else {
            updatedArgs = arguments;
        }

        return originalSelect2.apply(this, updatedArgs);
    };

})(AJS.$);
//fgnass.github.com/spin.js#v1.2.7
/*
    Modified by Atlassian
 */
!function(window, document, undefined) {

  /**
   * Copyright (c) 2011 Felix Gnass [fgnass at neteye dot de]
   * Licensed under the MIT license
   */

  var prefixes = ['webkit', 'Moz', 'ms', 'O'] /* Vendor prefixes */
    , animations = {} /* Animation rules keyed by their name */
    , useCssAnimations

  /**
   * Utility function to create elements. If no tag name is given,
   * a DIV is created. Optionally properties can be passed.
   */
  function createEl(tag, prop) {
    var el = document.createElement(tag || 'div')
      , n

    for(n in prop) el[n] = prop[n]
    return el
  }

  /**
   * Appends children and returns the parent.
   */
  function ins(parent /* child1, child2, ...*/) {
    for (var i=1, n=arguments.length; i<n; i++)
      parent.appendChild(arguments[i])

    return parent
  }

  /**
   * Insert a new stylesheet to hold the @keyframe or VML rules.
   */
  var sheet = function() {
    var el = createEl('style', {type : 'text/css'})
    ins(document.getElementsByTagName('head')[0], el)
    return el.sheet || el.styleSheet
  }()

  /**
   * Creates an opacity keyframe animation rule and returns its name.
   * Since most mobile Webkits have timing issues with animation-delay,
   * we create separate rules for each line/segment.
   */
  function addAnimation(alpha, trail, i, lines) {
    var name = ['opacity', trail, ~~(alpha*100), i, lines].join('-')
      , start = 0.01 + i/lines*100
      , z = Math.max(1 - (1-alpha) / trail * (100-start), alpha)
      , prefix = useCssAnimations.substring(0, useCssAnimations.indexOf('Animation')).toLowerCase()
      , pre = prefix && '-'+prefix+'-' || ''

    if (!animations[name]) {
      sheet.insertRule(
        '@' + pre + 'keyframes ' + name + '{' +
        '0%{opacity:' + z + '}' +
        start + '%{opacity:' + alpha + '}' +
        (start+0.01) + '%{opacity:1}' +
        (start+trail) % 100 + '%{opacity:' + alpha + '}' +
        '100%{opacity:' + z + '}' +
        '}', sheet.cssRules.length)

      animations[name] = 1
    }
    return name
  }

  /**
   * Tries various vendor prefixes and returns the first supported property.
   **/
  function vendor(el, prop) {
    var s = el.style
      , pp
      , i

    if(s[prop] !== undefined) return prop
    prop = prop.charAt(0).toUpperCase() + prop.slice(1)
    for(i=0; i<prefixes.length; i++) {
      pp = prefixes[i]+prop
      if(s[pp] !== undefined) return pp
    }
  }

  /**
   * Sets multiple style properties at once.
   */
  function css(el, prop) {
    for (var n in prop)
      el.style[vendor(el, n)||n] = prop[n]

    return el
  }

  /**
   * Fills in default values.
   */
  function merge(obj) {
    for (var i=1; i < arguments.length; i++) {
      var def = arguments[i]
      for (var n in def)
        if (obj[n] === undefined) obj[n] = def[n]
    }
    return obj
  }

  /**
   * Returns the absolute page-offset of the given element.
   */
  function pos(el) {
    var o = { x:el.offsetLeft, y:el.offsetTop }
    while((el = el.offsetParent))
      o.x+=el.offsetLeft, o.y+=el.offsetTop

    return o
  }

  var defaults = {
    lines: 12,            // The number of lines to draw
    length: 7,            // The length of each line
    width: 5,             // The line thickness
    radius: 10,           // The radius of the inner circle
    rotate: 0,            // Rotation offset
    corners: 1,           // Roundness (0..1)
    color: '#000',        // #rgb or #rrggbb
    speed: 1,             // Rounds per second
    trail: 100,           // Afterglow percentage
    opacity: 1/4,         // Opacity of the lines
    fps: 20,              // Frames per second when using setTimeout()
    zIndex: 2e9,          // Use a high z-index by default
    className: 'spinner', // CSS class to assign to the element
    top: 'auto',          // center vertically
    left: 'auto',         // center horizontally
    position: 'relative'  // element position
  }

  /** The constructor */
  var Spinner = function Spinner(o) {
    if (!this.spin) return new Spinner(o)
    this.opts = merge(o || {}, Spinner.defaults, defaults)
  }

  Spinner.defaults = {}

  merge(Spinner.prototype, {
    spin: function(target) {
      this.stop()
      var self = this
        , o = self.opts
        , el = self.el = css(createEl(0, {className: o.className}), {position: o.position, width: 0, zIndex: o.zIndex})
        , mid = o.radius+o.length+o.width
        , ep // element position
        , tp // target position

      if (target) {
        target.insertBefore(el, target.firstChild||null)
        tp = pos(target)
        ep = pos(el)
        css(el, {
          left: (o.left == 'auto' ? tp.x-ep.x + (target.offsetWidth >> 1) : parseInt(o.left, 10) + mid) + 'px',
          top: (o.top == 'auto' ? tp.y-ep.y + (target.offsetHeight >> 1) : parseInt(o.top, 10) + mid)  + 'px'
        })
      }

      el.setAttribute('aria-role', 'progressbar')
      self.lines(el, self.opts)

      if (!useCssAnimations) {
        // No CSS animation support, use setTimeout() instead
        var i = 0
          , fps = o.fps
          , f = fps/o.speed
          , ostep = (1-o.opacity) / (f*o.trail / 100)
          , astep = f/o.lines

        ;(function anim() {
          i++;
          for (var s=o.lines; s; s--) {
            var alpha = Math.max(1-(i+s*astep)%f * ostep, o.opacity)
            self.opacity(el, o.lines-s, alpha, o)
          }
          self.timeout = self.el && setTimeout(anim, ~~(1000/fps))
        })()
      }
      return self
    },

    stop: function() {
      var el = this.el
      if (el) {
        clearTimeout(this.timeout)
        if (el.parentNode) el.parentNode.removeChild(el)
        this.el = undefined
      }
      return this
    },

    lines: function(el, o) {
      var i = 0
        , seg

      function fill(color, shadow) {
        return css(createEl(), {
          position: 'absolute',
          width: (o.length+o.width) + 'px',
          height: o.width + 'px',
          background: color,
          boxShadow: shadow,
          transformOrigin: 'left',
          transform: 'rotate(' + ~~(360/o.lines*i+o.rotate) + 'deg) translate(' + o.radius+'px' +',0)',
          borderRadius: (o.corners * o.width>>1) + 'px'
        })
      }

      for (; i < o.lines; i++) {
        seg = css(createEl(), {
          position: 'absolute',
          top: 1+~(o.width/2) + 'px',
          transform: o.hwaccel ? 'translate3d(0,0,0)' : '',
          opacity: o.opacity,
          animation: useCssAnimations && addAnimation(o.opacity, o.trail, i, o.lines) + ' ' + 1/o.speed + 's linear infinite'
        })

        if (o.shadow) ins(seg, css(fill('#000', '0 0 4px ' + '#000'), {top: 2+'px'}))

        ins(el, ins(seg, fill(o.color, '0 0 1px rgba(0,0,0,.1)')))
      }
      return el
    },

    opacity: function(el, i, val) {
      if (i < el.childNodes.length) el.childNodes[i].style.opacity = val
    }

  })

  /////////////////////////////////////////////////////////////////////////
  // VML rendering for IE
  /////////////////////////////////////////////////////////////////////////

  /**
   * Check and init VML support
   */
  ;(function() {

    function vml(tag, attr) {
      return createEl('<' + tag + ' xmlns="urn:schemas-microsoft.com:vml" class="spin-vml">', attr)
    }

    var s = css(createEl('group'), {behavior: 'url(#default#VML)'})

    if (!vendor(s, 'transform') && s.adj) {

      // VML support detected. Insert CSS rule ...
      sheet.addRule('.spin-vml', 'behavior:url(#default#VML)')

      Spinner.prototype.lines = function(el, o) {
        var r = o.length+o.width
          , s = 2*r

        function grp() {
          return css(
            vml('group', {
              coordsize: s + ' ' + s,
              coordorigin: -r + ' ' + -r
            }),
            { width: s, height: s }
          )
        }

        var margin = -(o.width+o.length)*2 + 'px'
          , g = css(grp(), {position: 'absolute', top: margin, left: margin})
          , i

        function seg(i, dx, filter) {
          ins(g,
            ins(css(grp(), {rotation: 360 / o.lines * i + 'deg', left: ~~dx}),
              ins(css(vml('roundrect', {arcsize: o.corners}), {
                  width: r,
                  height: o.width,
                  left: o.radius,
                  top: -o.width>>1,
                  filter: filter
                }),
                vml('fill', {color: o.color, opacity: o.opacity}),
                vml('stroke', {opacity: 0}) // transparent stroke to fix color bleeding upon opacity change
              )
            )
          )
        }

        if (o.shadow)
          for (i = 1; i <= o.lines; i++)
            seg(i, -2, 'progid:DXImageTransform.Microsoft.Blur(pixelradius=2,makeshadow=1,shadowopacity=.3)')

        for (i = 1; i <= o.lines; i++) seg(i)
        return ins(el, g)
      }

      Spinner.prototype.opacity = function(el, i, val, o) {
        var c = el.firstChild
        o = o.shadow && o.lines || 0
        if (c && i+o < c.childNodes.length) {
          c = c.childNodes[i+o]; c = c && c.firstChild; c = c && c.firstChild
          if (c) c.opacity = val
        }
      }
    }
    else
      useCssAnimations = vendor(s, 'animation')
  })()

  //ATLASSIAN - always expose the global for and don't check define.amd for products who deleted it.
  if (typeof define == 'function')
    define('aui/internal/spin', [],function() { return Spinner });

  window.Spinner = Spinner

}(window, document);

define('aui/button',['aui/internal/spin'], function (Spinner) {
    'use strict';

    /*
     Can't depend on jQuery or skate since they won't be defined in product environments (thanks to deletion of the define.amd object)
     */
    var $ = AJS.$;
    var skate = window.skate;

    function createSpinner(button) {
        var $button = $(button);
        var spinContainer = document.createElement('div');
        var $spinContainer = $(spinContainer);
        $spinContainer.addClass('aui-button-spinner');

        var presets = {color: $button.css('color'), lines: 12, length: 3, width: 2, radius: 3, trail: 60, speed: 1.5};
        $spinContainer.data().spinner = new Spinner(presets).spin(spinContainer);

        $button.append(spinContainer);
    }

    function removeSpinner(button) {
        var $button = $(button);
        var $spinContainer = $button.children('.aui-button-spinner').first();

        var data = $spinContainer.data();
        data.spinner.stop();
        delete data.spinner;

        $spinContainer.remove();
    }

    function isBusy(button) {
        var ariaBusy = button.getAttribute('aria-busy');
        return ariaBusy && ariaBusy === 'true';
    }

    function isInputNode(button) {
        return button.nodeName === 'INPUT';
    }

    skate('aui-button', {
        type: skate.types.CLASS,

        ready: function(element) {
            if (!element.hasAttribute('aria-busy')) {
                element.setAttribute('aria-busy', false);
            }
        },

        prototype: {
            /**
             * Adds a spinner to the button and hides the text
             *
             * @returns {HTMLElement}
             */
            busy: function() {
                if (isInputNode(this) || isBusy(this)) {
                    AJS.warn('It is not valid to call busy() on an input button.');
                    return this;
                }

                createSpinner(this);
                this.setAttribute('aria-busy', true);

                return this;
            },

            /**
             * Removes the spinner and shows the tick on the button
             *
             * @returns {HTMLElement}
             */
            idle: function() {
                if (isInputNode(this) || !isBusy(this)) {
                    AJS.warn('It is not valid to call idle() on an input button.');
                    return this;
                }

                removeSpinner(this);
                this.setAttribute('aria-busy', false);

                return this;
            },

            /**
             * Removes the spinner and shows the tick on the button
             *
             * @returns {Boolean}
             */
            isBusy: function() {
                if (isInputNode(this)) {
                    AJS.warn('It is not valid to call isBusy() on an input button.');
                    return false;
                }

                return isBusy(this);
            }
        }
    });
});

;(function(init) {
    'use strict';

    define('aui/trigger',[],function() {
        return init(AJS.$, window.skate);
    });
})(function ($, skate) {
    'use strict';


    function findControlled(trigger) {
        return document.getElementById(trigger.getAttribute('aria-controls'));
    }

    function triggerMessage(trigger, e) {
        if (trigger.isEnabled()) {
            var component = findControlled(trigger);
            if (component && component.message) {
                component.message(e);
            }
        }
    }


    skate('data-aui-trigger', {
        type: skate.types.ATTR,
        insert: function(trigger) {
            $(trigger).on({
                click: function(e) {
                    triggerMessage(trigger, e);
                    e.preventDefault();
                },
                'mouseenter mouseleave focus blur': function(e) {
                    triggerMessage(trigger, e);
                }
            });
        },
        prototype: {
            disable: function() {
                this.setAttribute('aria-disabled', 'true');
            },
            enable: function() {
                this.setAttribute('aria-disabled', 'false');
            },
            isEnabled: function() {
                return this.getAttribute('aria-disabled') !== 'true';
            }
        }
    });
});

;(function(init) {
    'use strict';

    define('aui/inline-dialog2',['./internal/alignment', './trigger'], function(Alignment) {
        return init(Alignment, AJS.layer, window.skate);
    });
})(function(Alignment, Layer, skate) {
    'use strict';


    var DEFAULT_HOVEROUT_DELAY = 1000;


    function getTrigger (element) {
        return document.querySelector('[aria-controls="' + element.id + '"]');
    }

    function doIfTrigger (element, callback) {
        var trigger = getTrigger(element);

        if (trigger) {
            callback(trigger);
        }
    }

    function doIfHover (element, callback) {
        var isHover = element.getAttribute('data-aui-responds-to') === 'hover';
        if (isHover) {
            callback(element);
        }
    }

    function initAlignment (element, trigger) {
        if (!element._auiAlignment) {
            element._auiAlignment = new Alignment(element, trigger);
        }
    }

    function enableAlignment (element, trigger) {
        initAlignment(element, trigger);
        element._auiAlignment.enable();
    }

    function disableAlignment (element, trigger) {
        initAlignment(element, trigger);
        element._auiAlignment.disable();
    }


    skate('aui-inline-dialog2', {
        type: skate.types.TAG,

        ready: function(element) {
            doIfTrigger(element, function (trigger) {
                trigger.setAttribute('aria-expanded', element.isVisible());
                trigger.setAttribute('aria-haspopup', 'true');
            });

            doIfHover(element, function () {
                element.mouseInside = false;

                element.addEventListener('mouseenter', function () {
                    element.mouseInside = true;
                    element.message({type: 'mouseenter'});
                });
                element.addEventListener('mouseleave', function () {
                    element.mouseInside = false;
                    element.message({type: 'mouseleave'});
                });
            });
        },

        remove: function (element) {
            if (element._auiAlignment) {
                element._auiAlignment.destroy();
            }
        },

        prototype: {
            /**
             * Removes the inline dialog after hiding and removing it from the layer stack.
             *
             * @returns {undefined}
             */
            remove: function() {
                Layer(this).remove();
            },

            /**
             * Shows the inline dialog.
             *
             * @returns {HTMLElement}
             */
            show: function() {
                var that = this;

                Layer(this).show();

                doIfTrigger(this, function (trigger) {
                    enableAlignment(that, trigger);
                    trigger.setAttribute('aria-expanded', 'true');
                });

                return this;
            },

            /**
             * Hides the inline dialog.
             *
             * @returns {HTMLElement}
             */
            hide: function() {
                var that = this;

                Layer(this).hide();

                doIfTrigger(this, function (trigger) {
                    disableAlignment(that, trigger);
                    trigger.setAttribute('aria-expanded', 'false');
                });

                return this;
            },

            /**
             * Returns whether or not the inline dialog is visible.
             *
             * @returns {HTMLElement}
             */
            isVisible: function() {
                return Layer(this).isVisible();
            },

            /**
             * Handles the receiving of a message from another component.
             *
             * @param {Object} message The message to act on.
             *
             * @returns {HTMLElement}
             */
            message: function(message) {
                handleMessage(this, message);
                return this;
            }
        }
    });

    function handleMessage (inlineDialog, message) {
        var messageTypeMap = {
            toggle: ['click'],
            hover: ['mouseenter', 'mouseleave', 'focus', 'blur']
        };
        var respondsTo = inlineDialog.getAttribute('data-aui-responds-to');
        var messageList = messageTypeMap[respondsTo];

        if (messageList && messageList.indexOf(message.type) > -1) {
            messageHandler[message.type](inlineDialog, message);
        }
    }

    var messageHandler = {
        click: function (inlineDialog) {
            if (inlineDialog.isVisible()) {
                if (!Layer(inlineDialog).isPersistent()) {
                    inlineDialog.hide();
                }
            } else {
                inlineDialog.show();
            }
        },

        mouseenter: function (inlineDialog) {
            if (!inlineDialog.isVisible()) {
                inlineDialog.show();
            }

            if (inlineDialog._clearMouseleaveTimeout) {
                inlineDialog._clearMouseleaveTimeout();
            }
        },

        mouseleave: function (inlineDialog) {
            if (Layer(inlineDialog).isPersistent() || !inlineDialog.isVisible()) {
                return;
            }

            if (inlineDialog._clearMouseleaveTimeout) {
                inlineDialog._clearMouseleaveTimeout();
            }

            var timeout = setTimeout(function () {
                if (!inlineDialog.mouseInside) {
                    inlineDialog.hide();
                }
            }, DEFAULT_HOVEROUT_DELAY);

            inlineDialog._clearMouseleaveTimeout = function () {
                clearTimeout(timeout);
                inlineDialog._clearMouseleaveTimeout = null;
            };
        },

        focus: function (inlineDialog) {
            if (!inlineDialog.isVisible()) {
                inlineDialog.show();
            }
        },

        blur: function (inlineDialog) {
            if (!Layer(inlineDialog).isPersistent() && inlineDialog.isVisible()) {
                inlineDialog.hide();
            }
        }
    };
});

/*
 * Ideas from https://gist.github.com/its-florida/1290439 are acknowledged and used here.
 * Resulting file is heavily modified from that gist so is licensed under AUI's license.
 *
 * You can now create a spinner using any of the variants below:
 *
 * $("#el").spin(); // Produces default Spinner using the text color of #el.
 * $("#el").spin("small"); // Produces a 'small' Spinner using the text color of #el.
 * $("#el").spin("large", { ... }); // Produces a 'large' Spinner with your custom settings.
 * $("#el").spin({ ... }); // Produces a Spinner using your custom settings.
 *
 * $("#el").spin(false); // Kills the spinner.
 * $("#el").spinStop(); // Also kills the spinner.
 *
 */
(function($) {
    $.fn.spin = function(optsOrPreset, opts) {
        var preset, options;

        if (typeof optsOrPreset === 'string') {
            if (! optsOrPreset in $.fn.spin.presets) {
                throw new Error("Preset '" + optsOrPreset + "' isn't defined");
            }
            preset = $.fn.spin.presets[optsOrPreset];
            options = opts || {};
        } else {
            if (opts) {
                throw new Error('Invalid arguments. Accepted arguments:\n' +
                    '$.spin([String preset[, Object options]]),\n' +
                    '$.spin(Object options),\n' +
                    '$.spin(Boolean shouldSpin)');
            }
            preset = $.fn.spin.presets.small;
            options = $.isPlainObject(optsOrPreset) ? optsOrPreset : {};
        }

        if (window.Spinner) {
            return this.each(function() {
                var $this = $(this),
                    data = $this.data();

                if (data.spinner) {
                    data.spinner.stop();
                    delete data.spinner;
                }

                if (optsOrPreset === false) { // just stop it spinning.
                    return;
                }

                options = $.extend({ color: $this.css('color') }, preset, options);
                data.spinner = new Spinner(options).spin(this);
            });
        } else {
            throw "Spinner class not available.";
        }
    };
    $.fn.spin.presets = {
        "small": { lines: 12, length: 3, width: 2, radius: 3, trail: 60, speed: 1.5 },
        "medium": { lines: 12, length: 5, width: 3, radius: 8, trail: 60, speed: 1.5 },
        "large": { lines: 12, length: 8, width: 4, radius: 10, trail: 60, speed: 1.5 }
    };

    $.fn.spinStop = function() {
        if (window.Spinner) {
            return this.each(function() {
                var $this = $(this),
                    data = $this.data();

                if (data.spinner) {
                    data.spinner.stop();
                    delete data.spinner;
                }

            });
        } else {
            throw "Spinner class not available.";
        }
    };
})(jQuery);
;(function(init) {
    'use strict';

    define('aui/flag',[],function() {
        return init(AJS, AJS.$, AJS.template);
    });
})(function (AJS, $, template) {
    'use strict';

    var AUTO_CLOSE_TIME = 5000;
    var ID_FLAG_CONTAINER = 'aui-flag-container';
    var defaultOptions = {
            body: '',
            persistent: false,
            close: '',
            title: '',
            type: 'info'
        };

    function flag (options) {

        //TODO remove deprecation warnings in AUI-2894
        if (options.hasOwnProperty('persistent')) {
            var persistentDeprecationLogger;
            if (options.persistent) {
                persistentDeprecationLogger = AJS.deprecate.getMessageLogger('persistent: true', '5.8.0', {alternativeName: 'close: never', extraInfo: 'See https://ecosystem.atlassian.net/browse/AUI-2893.'});
            } else if (!options.persistent) {
                persistentDeprecationLogger = AJS.deprecate.getMessageLogger('persistent: false', '5.8.0', {alternativeName: 'close: manual', extraInfo: 'See https://ecosystem.atlassian.net/browse/AUI-2893.'});
            }
            persistentDeprecationLogger();
        }

        options = $.extend({}, defaultOptions, options);

        var isCloseable = options.close !== 'never' && (options.close || (options.persistent !== true));
        var $flag = renderFlagElement(options, isCloseable); //TODO refactor isCloseable out with AUI-2894
        extendFlagElement($flag);

        if (options.close === 'auto') {
            makeCloseable($flag);
            makeAutoClosable($flag);
        } else if (options.close === 'manual') {
            makeCloseable($flag);
        } else if (options.close === 'never') {
            //do nothing
        } else if (!options.persistent) { //TODO remove with AUI-2894
            makeCloseable($flag);
        }



        pruneFlagContainer();

        return insertFlag($flag);
    }

    function extendFlagElement ($flag) {
        var flag = $flag[0];

        flag.close = function () {
            closeFlag($flag);
        };
    }

    function renderFlagElement (options, isCloseable) {
        var html =
            '<div class="aui-flag">' +
                '<div class="aui-message aui-message-{type} {type} {closeable} shadowed">' +
                    '<p class="title">' +
                    '<strong>{title}</strong>' +
                    '</p>' +
                    '{body}<!-- .aui-message -->' +
                '</div>' +
            '</div>';
        var rendered = template(html).fill({
                'body:html': options.body || '',
                closeable: isCloseable ? 'closeable' : '',
                title: options.title || '',
                type: options.type
            }).toString();

        return $(rendered);
    }

    function makeCloseable ($flag) {
        var $icon = $('<span class="aui-icon icon-close" role="button" tabindex="0"></span>');

        $icon.click(function () {
            closeFlag($flag);
        });

        $icon.keypress(function(e) {
            if ((e.which === AJS.keyCode.ENTER) || (e.which === AJS.keyCode.SPACE)) {
                closeFlag($flag);
                e.preventDefault();
            }
        });

        return $flag.find('.aui-message').append($icon)[0];
    }

    function makeAutoClosable ($flag) {
        setTimeout(function () {
            $flag[0].close();
        }, AUTO_CLOSE_TIME);
    }

    function closeFlag ($flagToClose) {
        var flag = $flagToClose.get(0);

        flag.setAttribute('aria-hidden', 'true');
        flag.dispatchEvent(new CustomEvent('aui-flag-close', { bubbles: true }));

        return flag;
    }

    function pruneFlagContainer () {
        var $container = findContainer();
        var $allFlags = $container.find('.aui-flag');

        $allFlags.get().forEach(function(flag) {
            var isFlagAriaHidden = flag.getAttribute('aria-hidden') === 'true';

            if (isFlagAriaHidden) {
                $(flag).remove();
            }
        });
    }

    function findContainer () {
        return $('#' + ID_FLAG_CONTAINER);
    }

    function insertFlag ($flag) {
        var $flagContainer = findContainer();

        if (!$flagContainer.length) {
            $flagContainer = $('<div id="' + ID_FLAG_CONTAINER + '"></div>');
            $('body').prepend($flagContainer);
        }

        $flag.appendTo($flagContainer);
        AJS._internal.animation.recomputeStyle($flag);

        return $flag.attr('aria-hidden', 'false')[0];
    }

    return flag;
});

;(function(init) {
    'use strict';

    define('aui/banner',[],function() {
        return init(AJS, AJS.$, AJS.template);
    });
})(function (AJS, $, template) {
    'use strict';

    var ID_BANNER_CONTAINER = 'header';

    function banner (options) {
        var $banner = renderBannerElement(options);

        pruneBannerContainer();
        insertBanner($banner);
        return $banner;
    }

    function renderBannerElement(options) {
        var html =
            '<div class="aui-banner aui-banner-{type}" role="banner">' +
                '<p class="aui-banner-title" role="heading">' +
                    '{title}' +
                '</p>' +
                '{body}' +
            '</div>';

        var $banner = $(template(html).fill({
            'type': 'error',
            'title': options.title || '',
            'body:html': options.body || ''
        }).toString());

        $banner.find('.aui-banner-title:empty').remove();

        return $banner;
    }

    function pruneBannerContainer() {
        var $container = findContainer();
        var $allBanners = $container.find('.aui-banner');

        $allBanners.get().forEach(function(banner) {
            var isBannerAriaHidden = banner.getAttribute('aria-hidden') === 'true';
            if (isBannerAriaHidden) {
                $(banner).remove();
            }
        });
    }

    function findContainer() {
        return $('#' + ID_BANNER_CONTAINER);
    }

    function insertBanner($banner) {
        var $bannerContainer = findContainer();
        if (!$bannerContainer.length) {
            throw new Error('You must implement the application header');
        }

        $banner.prependTo($bannerContainer);
        AJS._internal.animation.recomputeStyle($banner);
        $banner.attr('aria-hidden', 'false');
    }

    return banner;
});

/**
 * Enhances a <select multiple> to be a button & dropdown combination. The button lists the selected <option>s. Clicking
 * the button reveals dropdown that has a list of checkboxes. Checking/Unchecking update the button's value and the state
 * of the corresponding <option>.
 *
 * To use simple add the attr aui-checkbox-multiselect to a <select multiple>.
 */
define('aui/checkbox-multiselect',[],function () {

    var skate = window.skate;
    var $ = window.jQuery;

    /**
     * Set of soy templates
     */
    var templates = {
        dropdown: function (items) {

            function createSection() {
                return $('<div class="aui-dropdown2-section">');
            }

            // clear items section
            var $clearItemsSection = createSection();

            $('<button />').attr({
                'type': 'button',
                'data-aui-checkbox-multiselect-clear': '',
                'class': 'aui-button aui-button-link'
            })
                .text(AJS.I18n.getText('aui.checkboxmultiselect.clear.selected'))
                .appendTo($clearItemsSection);

            // list of items
            var $section = createSection();
            var $itemList = $('<ul />').appendTo($section);

            $.each(items, function (idx, item) {

                var $li = $('<li />').attr({
                    'class': item.styleClass || ''
                }).appendTo($itemList);

                var $a =  $('<a />')
                    .text(item.label)
                    .attr('data-value', item.value)
                    .addClass('aui-dropdown2-checkbox aui-dropdown2-interactive')
                    .appendTo($li);

                if (item.icon) {
                    $('<span />')
                        .addClass('aui-icon')
                        .css("backgroundImage", 'url(' + item.icon + ')")')
                        .appendTo($a);
                }

                if (item.selected) {
                    $a.addClass('aui-dropdown2-checked')
                }
            });

            return $('<div />')
                .append($clearItemsSection)
                .append($section)
                .html();
        },
        furniture: function (name, optionsHtml) {

            var dropdownId = name + '-dropdown';

            var $select = $('<select />').attr({
                'name': name,
                'multiple': 'multiple'
            }).html(optionsHtml);

            var $dropdown = $('<div>')
                .attr({
                    'id': dropdownId,
                    'class': 'aui-checkbox-multiselect-dropdown aui-dropdown2 aui-style-default'
                });

            var $button = $('<button />')
                .attr({
                    'class': 'aui-checkbox-multiselect-btn aui-button aui-dropdown2-trigger',
                    'type': 'button',
                    'aria-owns': dropdownId,
                    'aria-haspopup': true
                });

            return $('<div />')
                .append($select)
                .append($button)
                .append($dropdown)
                .html();

        }
    };

    /**
     * Handles when user clicks an item in the dropdown list. Either selects or unselects the corresponding
     * option in the <select>.
     * @private
     */
    function handleDropdownSelection(e) {
        var $a = $(e.target);
        var value = $a.attr('data-value');
        updateOption(this, value, $a.hasClass('aui-dropdown2-checked'));
    }

    /**
     * Selects or unselects the <option> corresponding the given value.
     * @private
     * @param component - Checkbox MultiSelect web component
     * @param value - value of option to update
     * @param {Boolean} selected - select or unselect it.
     */
    function updateOption(component, value, selected) {
        var $toUpdate = component.$select.find('option').filter(function () {
            var $this = $(this);
            return $this.attr('value') === value && $this.prop('selected') != selected;
        });
        if ($toUpdate.length) {
            $toUpdate.prop('selected', selected);
            component.$select.trigger('change');
        }
    }

    /**
     * Enables 'clear all' button if there are any selected <option>s, otherwise disables it.
     * @private
     */
    function updateClearAll(component) {
        component.$dropdown.find('[data-aui-checkbox-multiselect-clear]').prop('disabled', function () {
            return getSelectedDescriptors(component).length < 1;
        })
    }

    /**
     * Gets button title used for tipsy. Is blank when dropdown is open so we don't get tipsy hanging over options.
     * @private
     * @param component
     * @returns {string}
     */
    function getButtonTitle(component) {
        return component.$dropdown.is('[aria-hidden=false]') ? '' : getSelectedLabels(component).join(', ');
    }

    /**
     * Converts a jQuery collection of <option> elements into an object that describes them.
     * @param {jQuery} $options
     * @returns {Array<Object>}
     * @private
     */
    function mapOptionDescriptors($options) {
        return $options.map(function () {
            var $option = $(this);
            return {
                value: $option.val(),
                label: $option.text(),
                icon: $option.data('icon'),
                styleClass: $option.data('styleClass'),
                title: $option.attr('title'),
                disabled: $option.attr('disabled'),
                selected: $option.attr('selected')
            };
        });
    }

    /**
     * Gets label for when nothing is selected
     * @returns {string}
     * @private
     */
    function getImplicitAllLabel (component) {
        return $(component).data('allLabel') || 'All';
    }


    /**
     * Renders dropdown with list of items representing the selected or unselect state of the <option>s in <select>
     * @private
     */
    function renderDropdown(component) {
        component.$dropdown.html(templates.dropdown(getDescriptors(component)));
        updateClearAll(component);
    }

    /**
     * Renders button with the selected <option>'s innerText in a comma seperated list. If nothing is selected 'All'
     * is displayed.
     * @private
     */
    function renderButton(component) {
        var selectedLabels = getSelectedLabels(component);
        var label = isImplicitAll(component) ? getImplicitAllLabel(component) : selectedLabels.join(', ');
        component.$btn.text(label);
    }

    /**
     * Gets descriptor for selected options, the descriptor is an object that contains meta information about
     * the option, such as value, label, icon etc.
     * @private
     * @returns Array<Object>
     */
    function getDescriptors(component) {
        return mapOptionDescriptors(component.getOptions());
    }

    /**
     * Gets descriptor for selected options, the descriptor is an object that contains meta information about
     * the option, such as value, label, icon etc.
     * @private
     * @returns Array<Object>
     */
    function getSelectedDescriptors(component) {
        return mapOptionDescriptors(component.getSelectedOptions());
    }

    /**
     * Gets the innerText of the selected options
     * @private
     * @returns Array<String>
     */
    function getSelectedLabels(component) {
        return $.map(getSelectedDescriptors(component), function (descriptor) {
            return descriptor.label;
        });
    }

    /**
     * If nothing is selected, we take this to mean that everything is selected.
     * @returns Boolean
     */
    function isImplicitAll(component) {
        return getSelectedDescriptors(component).length === 0;
    }

    skate('aui-checkbox-multiselect', {
        type: skate.types.TAG,
        template: function (component) {
            var name = component.getAttribute("name") || AJS.id('aui-checkbox-multiselect-');
            component.innerHTML = templates.furniture(name, component.innerHTML);
        },
        ready: function (component) {
            component.$select = $("select", component)
                .change(function () {
                    renderButton(component);
                    updateClearAll(component);
                });

            component.$dropdown = $(".aui-checkbox-multiselect-dropdown", component)
                .on('aui-dropdown2-item-check', handleDropdownSelection.bind(component))
                .on('aui-dropdown2-item-uncheck', handleDropdownSelection.bind(component))
                .on('click', 'button[data-aui-checkbox-multiselect-clear]', component.deselectAllOptions.bind(component));

            component.$btn = $(".aui-checkbox-multiselect-btn", component)
                .tooltip({
                    title: function () {
                        return getButtonTitle(component);
                    }
                });

            renderButton(component);
            renderDropdown(component);
        },
        prototype: {

            /**
             * Gets all options regardless of selected or unselected
             * @returns {jQuery}
             */
            getOptions: function () {
                return this.$select.find('option');
            },

            /**
             * Gets all selected options
             * @returns {jQuery}
             */
            getSelectedOptions: function () {
                return this.$select.find('option:selected');
            },

            /**
             * Sets <option> elements matching given value to selected
             */
            selectOption: function (value) {
                updateOption(this, value, true);
            },

            /**
             * Sets <option> elements matching given value to unselected
             */
            unselectOption: function (value) {
                updateOption(this, value, false);
            },

            /**
             * Gets value of <select>
             * @returns Array
             */
            getValue: function () {
                return this.$select.val();
            },

            /**
             * Unchecks all items in the dropdown and in the <select>
             */
            deselectAllOptions: function () {
                this.$select.val([]).trigger('change');
                this.$dropdown.find('.aui-dropdown2-checked,.checked').removeClass('aui-dropdown2-checked checked');
            },
            /**
             * Adds an option to the <select>
             * @param descriptor
             */
            addOption: function (descriptor) {
                $("<option />").attr({
                    value: descriptor.value,
                    icon: descriptor.icon,
                    disabled: descriptor.disabled,
                    selected: descriptor.selected,
                    title: descriptor.title
                })
                    .text(descriptor.label)
                    .appendTo(this.$select);
                renderButton(this);
                renderDropdown(this);
            },
            /**
             * Removes options matching value from <select>
             * @param value
             */
            removeOption: function (value) {
                this.$select.find("[value='" + value + "']").remove();
                renderButton(this);
                renderDropdown(this);
            }
        }
    });

});
;(function(init) {
    'use strict';

    define('aui/polyfills/placeholder',[],function () {
        return init(AJS.$, window.skate);
    });
})(function ($, skate) {
    'use strict';

    if('placeholder' in document.createElement('input')) {
        return;
    }

    skate('placeholder', {
        type: skate.types.ATTR,
        insert: function(el) {
            giveInputPlaceholderPolyfill(el);
        }
    });

    function giveInputPlaceholderPolyfill(input) {
        var $input = $(input);
        applyDefaultText($input);

        $input.blur(function() {
            applyDefaultText($input);
        });

        $input.focus(function() {
            if($input.hasClass("aui-placeholder-shown")) {
                $input.val("");
                $input.removeClass("aui-placeholder-shown");
            }
        });
    }

    function applyDefaultText($input) {
        if(!$.trim($input.val()).length) {
            $input.val($input.attr("placeholder"));
            $input.addClass("aui-placeholder-shown");
        }
    }


});
