//------------------------------------------------------------------------------
// Initialization code
//------------------------------------------------------------------------------
function initSporx() {
    window.removeEventListener('load', initSporx, false);
    Sporx.init();
}
window.addEventListener('load', initSporx, false);

//------------------------------------------------------------------------------
// The Stash
//------------------------------------------------------------------------------
var Stash = {};

//------------------------------------------------------------------------------
// Sporx String Filters
//------------------------------------------------------------------------------
String.prototype.convertPerl5ToPerl6 = function() {
    return this.replace(/print/g, 'say');
}

String.prototype.current = function(regexp) {
    return this.replace(regexp, '{{#c|$1}}');
}

String.prototype.color = function(regexp) {
    return this.current(regexp);
}

//------------------------------------------------------------------------------
// Sporx code
//------------------------------------------------------------------------------
// The Sporx singleton object.
//
// Define just the getters and setters here since I don't know how after the
// object has been created. Can someone tell me?
var Sporx = {
    get offset() {
        return this._offset;
    },

    set offset(aValue) {
        this._offset = parseInt(aValue || 0);
        document.documentElement.setAttribute('lastoffset', this.offset);
        return this.offset;
    },

    get data() {
        if (!this._data) {
            // Make sure you break the text into parts smaller than 4096
            // characters, and name them as indicated. Tweak as required.
            // (What a hack. A JS programmer should find a better way.)
            // Luc St-Louis, and email is lucs@pobox.com.
            nodes = document.getElementById('builtinCode').childNodes;
            content = '';
            for (i in nodes) {
                if (nodes[i].nodeValue) {
                    content = content + nodes[i].nodeValue;
                }
            }
   
            this._data = this.splitSlides(content);
        }

        return this._data;
    },

    set data(aValue) {
        this._data = aValue;
        return aValue;
    },

    get isPresentationMode() {
        return (this.deck.selectedIndex == 0);
    },

    get dataPath() {
        if (!this._dataPath)
            this.dataPath = location.href;
        return this._dataPath;
    },

    set dataPath(aValue) {
        var oldDataPath = this._dataPath;
        this._dataPath = aValue;
        if (oldDataPath != aValue) {
            this._dataFolder =
                this._dataPath.split('?')[0].replace(/[^\/]+$/, '');
        }
        return this._dataPath;
    },

    get dataFolder() {
        if (!this._dataFolder)
            this.dataPath = this.dataPath;
        return this._dataFolder;
    },

    set dataFolder(aValue) {
        this._dataFolder = aValue;
        return this._dataFolder;
    }
};

Sporx.init = function(option) {
    this.size = 9;

    this._offset  = 0;
    this.canvas   = document.getElementById('canvas');
    this.content  = document.getElementById('content');
    this.textbox  = document.getElementById('textField');
    this.deck     = document.getElementById('deck');
    this.scroller = document.getElementById('scroller');

    this.toolbar         = document.getElementById('canvasToolbar');
    this.toolbarHeight   = this.toolbar.boxObject.height;
    this.isToolbarHidden = true;
    this.toolbar.setAttribute(
        'style',
        'margin-top:' + (0-this.toolbarHeight) + 'px;margin-bottom:0px;'
    );

    if (option) {
        for (var i in option) {
            this[i] = option[i];
        }
    }

    if (this.readParameter()) {
        this.takahashi();
    }

    document.documentElement.focus();
}

Sporx.takahashi = function() {
    this.preProcessSlides();
    this.setSporxTitle();

    if (!this.data[this.offset])
        this.offset = this.data.length - 1;

    document.getElementById("current_page").value = this.offset + 1;
    document.getElementById("max_page").value     = this.data.length;

    this.scroller.setAttribute('maxpos', this.data.length - 1);
    this.scroller.setAttribute('curpos', this.offset);

    var broadcaster = document.getElementById('canBack');
    if (!this.offset)
        broadcaster.setAttribute('disabled', true);
    else
        broadcaster.removeAttribute('disabled');

    var broadcaster = document.getElementById('canForward');
    if (this.offset == this.data.length - 1)
        broadcaster.setAttribute('disabled', true);
    else
        broadcaster.removeAttribute('disabled');

    this.canvas.setAttribute('rendering', true);

    var text = this.data[this.offset].
        replace(/^[\r\n]+/g,"").
        replace(/[\r\n]+$/g,"").
        replace(/(\r\n|[\r\n])/g,"\n").
        split('\n');

    var range = document.createRange();
    range.selectNodeContents(this.content);
    range.deleteContents();
    range.detach();

    var line;

    var labelId = 0;

    for (var i = 0; i < text.length; i++) {
        this.content.appendChild(document.createElement('hbox'));
        this.content.lastChild.setAttribute('align', 'center');
        this.content.lastChild.setAttribute('pack', 'center');

        line = text[i];
        image_width  = 0;
        image_height = 0;

        if (line.match(/^\*\s+/)) {
            var ul = document.createElementNS('http://www.w3.org/1999/xhtml', 'html:ul');
            while (line.match(/^\*\s+/)) {
                var li = document.createElementNS('http://www.w3.org/1999/xhtml', 'html:li');
                var line_text = line.replace(/^\*\s+/, '');
                // li.appendChild(document.createTextNode(line_text));
                li.appendChild(document.createElement('description'));
                this.inlineMarkupMess(line_text, li);
                ul.appendChild(li);
                i++;
                line = text[i];
                if (! line) line = '';
            }
            this.content.appendChild(ul);
            continue;
        }

        if (line.match(/^ /)) {
            this.content.lastChild.setAttribute('align', 'left');
            this.content.lastChild.setAttribute('class', 'pre');
            line = line.substring(1)
        }

        this.inlineMarkupMess(line, this.content);

        image_total_width = Math.max(image_total_width, image_width);
        image_total_height += image_height;
    }

    this.content.setAttribute('style', 'font-size:10px;');

    if (this.content.boxObject.width) {
        var canvas_w  = this.canvas.boxObject.width;
        var canvas_h  = this.canvas.boxObject.height-image_total_height;

        var content_w = this.content.boxObject.width;
        var new_fs = Math.round((canvas_w/content_w) * this.size);
        this.content.setAttribute('style', 'font-size:'+ new_fs + "px");

        if (this.content.boxObject.width < image_total_width) {
            content_w = image_total_width;
            new_fs = Math.round((canvas_w/content_w) * this.size);
            this.content.setAttribute('style', 'font-size:'+ new_fs + "px");
        }

        var content_h = this.content.boxObject.height;
        if (content_h >= canvas_h) {
            content_h = this.content.boxObject.height;
            new_fs = Math.round((canvas_h/content_h) * new_fs);
            this.content.setAttribute('style', 'font-size:'+ new_fs + "px");
        }
    }

    this.canvas.removeAttribute('rendering');
};

Sporx.inlineMarkupMess = function(line, content) {
    var uri;
    image_total_width  = 0;
    image_total_height = 0;
    var image_src;
    while (line.match(/^([^\{]+)?(\{\{ima?ge? +src="([^"]+)" +width="([0-9]+)" +height="([0-9]+)"[^\}]*\}\}|\{\{(([^\|]+)?\||)(.+?)\}\})(.+)?/)) {
        if (RegExp.$1) {
            content.lastChild.appendChild(
                document.createElement('description')
            );
            content.lastChild.lastChild.setAttribute(
                'value', RegExp.$1
            );
        }
        var newLine = line.substring((RegExp.$1+RegExp.$2).length);

        // Images
        if (/^([^\{]+)?\{\{ima?ge? +src="([^\"]+)" +width="([0-9]+)" +height="([0-9]+)"[^\}]*\}\}/.test(line)) {
            content.lastChild.
                appendChild(document.createElement('image'));
            var image_src = RegExp.$2;
            if (image_src.indexOf('http://') < 0 &&
                image_src.indexOf('https://') < 0)
                image_src = this.dataFolder+image_src;
            content.lastChild.lastChild.
                setAttribute('src', image_src);
            content.lastChild.lastChild.
                setAttribute('width', parseInt(RegExp.$3 || '0'));
            content.lastChild.lastChild.
                setAttribute('height', parseInt(RegExp.$4 || '0'));
            image_width  += parseInt(RegExp.$3 || '0');
            image_height = Math.max(image_height, parseInt(RegExp.$4 || '0'));
        }

        // Styles
        else if (/^([^\{]+)?\{\{(#([^\|]+)?\|)(.+?)\}\}/.test(line)) {
            uri = RegExp.$4;
            content.lastChild.
                appendChild(document.createElement('description'));
            content.lastChild.lastChild.
                setAttribute('value', uri);
            content.lastChild.lastChild.
                setAttribute('class', RegExp.$3);
        }

        // Links
        else if (/^([^\{]+)?\{\{(([^\|]+)?\||)([^\}]+)\}\}/.test(line)) {
            uri = RegExp.$4;
            if (uri.indexOf('://') < 0)
                uri = this.dataFolder+uri;
            content.lastChild.
                appendChild(document.createElement('description'));
            content.lastChild.lastChild.
                setAttribute('value', RegExp.$3 || RegExp.$4);
            content.lastChild.lastChild.
                setAttribute('href', uri);
            content.lastChild.lastChild.
                setAttribute('tooltiptext', uri);
            content.lastChild.lastChild.
                setAttribute('statustext', uri);
            content.lastChild.lastChild.
                setAttribute('class', 'link-text');
        }

        line = newLine;
    }

    if (line) {
        content.lastChild.appendChild(document.createElement('description'));
        content.lastChild.lastChild.setAttribute('value', line);
    }

}

Sporx.preProcessSlides = function() {
    if (this.preprocessed)
        return;
    this.preprocessed = true;
    this.removeCommentSlides();
    this.divideMultiPartSlides();
    this.compileSlidesToJavascriptFunctions();
    this.evalJavascriptToSlides();
}

// Comment slides begin with '#'
Sporx.removeCommentSlides = function() {
    this.transformSlides(
        function(slide) {
            if (slide.match(/^\n*$/) || slide.match(/^\s*#/))
                return [];
            return [slide];
        }
    );
}

// Divide slides into more slides if lines begin with '+'
Sporx.divideMultiPartSlides = function() {
    this.transformSlides(
        function(slide) {
            var slides = [];
            var part = '';
            while (slide.match(/([\s\S]*?\n)\+([\s\S]*)/)) {
                part += RegExp.$1;
                slides.push(part);
                slide = RegExp.$2;
            }
            slides.push(part + slide);
            return slides;
        }
    );
}

Sporx.mark_last_line_current = function(slide) {
    if (Stash._LAST_BULLET_MARKED_CURRENT) {
        slide = slide.replace(/(\*\s+)(.*)\n$/,
        function(m, a, b) {
            if (b.match(/\{\{/))
                return m;
            return a + '{{#c|' + b + '}}\n';
        });
    }
    return (slide);
}


// Turn each slide into a javascript function
Sporx.compileSlidesToJavascriptFunctions = function() {
    var self = this;
    this.transformSlides(
        function(slide) {
            slide = slide.replace(/^\n*/, '');
            if (slide.match(/^\$/))
                slide = self.compileJavascriptSlide(slide);
            else
                slide = self.compileNormalSlide(slide);
            return [slide];
        }
    );
}

Sporx.compileJavascriptSlide = function(slide) {
    var js = 'function() {\n';
    js += 'Stash.__ = Stash._;\n';
    js += 'Stash._ = "";\n';
    slide = slide.
    replace(
        /(^|\n)(\$\w+[^\S\n]*)\=[^\S\n]*\n([\s\S]*?)(?=\n\n|$)/g,
        function(m, x, y, code) {
            code = code.
                replace(/\n/g, '\\n\\\n');
            return x + y + ' = ' + '"' + code + '";\n';
        }
    ).
    replace(/(^|[^\$])\$\{([A-Z_]\w*)\}/g, '$1" + Stash.$2 + "').
    replace(/(^|[^\$])\$(?=[A-Z_])/g, '$1Stash.').
    replace(/\$\$(?=[A-Z_])/g, '$').
    replace(
        /\{\{.*?\}\}/g,
        function(m) {
            return m.replace(/"/g, '\\"');
        }
    );
    
    js += slide;
    js += 'return Stash._;\n';
    js += "}\n";
    return js;
}

Sporx.compileNormalSlide = function(slide) {
    slide = '$_ =\n' + slide;
    slide = slide.replace(/\$/g, '$');
    return this.compileJavascriptSlide(slide);
}

// Eval each slide
Sporx.evalJavascriptToSlides = function() {
    this.transformSlides(
        function(slide) {
            var result = '';
            try {
                // XXX('slide content:\n' + slide);
                var func = eval(slide);
                // XXX('javascript function:\n' + func);
                result = func.call();
                // XXX('result: >>' + result + '<<');
            }
            catch(e) {
                if (e == 'terminated...')
                    throw(e);
                XXX("Slide Javascript error:\n" + e + '\n' + slide);
                result = '';
            }

            if (result instanceof Array)
                result = result;
            else if (result.length)
                result = [result];
            else
                result = [];

            for (var i = 0; i < result.length; i++) {
                result[i] = String(result[i]).replace(/\\"/g, '"');
                result[i] = Sporx.mark_last_line_current(result[i]);
            }
            
            return result;
        }
    );
}

// Divide slides into more slides if lines begin with '+'
Sporx.transformSlides = function(func) {
    var slides = this.data;
    var transformed = [];
    for (var i = 0; i < slides.length; i++) {
        var slide = slides[i];
        var set = func(slide);
        for (var j = 0; j < set.length; j++)
            transformed.push(set[j]);
    }
    this.data = transformed;
}

Sporx.setSporxTitle = function() {
    if (!document.title) {
        document.title = this.data[0].
            replace(/[\r\n]/g, ' ').
            replace(/\{\{(.*\|)?\s*/g, '').
            replace(/\s*\}\}/g, '');
    }
}

Sporx.initializeData = function() {
    return this.splitSlides(this.textbox.value);
}

Sporx.splitSlides = function(text) {
    return text.
        replace(/\n__END__\r?\n[\s\S]*/m, '\n').
        replace(/&amp;/g, '&').
        replace(/&lt;/g, '<').
        split('----');
}

Sporx.reload = function() {
    if (this.dataPath != location.href) {
        var path = this.dataPath;
        if (location.href.match(/^https?:/)) {
            var request = new XMLHttpRequest();
            request.open('GET', path);
            request.onload = function() {
                Sporx.textbox.value = request.responseText;
                Sporx.data = Sporx.initializeData();

                Sporx.takahashi();

                path = null;
                request = null;
            };
            request.send(null);
        }
        else {
            document.getElementById('dataLoader').
                setAttribute('src', 'about:blank');
            window.setTimeout(function() {
                document.getElementById('dataLoader').
                    setAttribute('src', path);
                path = null;
            }, 10);
        }
    }
    else
        window.location.reload();
}

Sporx.forward = function() {
    this.offset++;
    if (location.hash && location.hash.match(/^#(\d+)$/)) {
        location = String(location).replace(/(\d+)$/, this.offset + 1);
    }
    this.takahashi();
}

Sporx.back = function() {
    this.offset--;
    if (this.offset < 0)
        this.offset = 0;
    if (location.hash && location.hash.match(/^#(\d+)$/)) {
        location = String(location).replace(/(\d+)$/, this.offset + 1);
    }
    this.takahashi();
}

Sporx.home = function() {
    this.offset = 0;
    this.takahashi();
}

Sporx.end = function() {
    this.offset = this.data.length - 1;
    this.takahashi();
}

Sporx.showPage = function(aPageOffset) {
    this.offset = aPageOffset ? aPageOffset : 0;
    this.takahashi();
}

Sporx.addPage = function() {
    if (this.textbox.value && !this.textbox.value.match(/(\r\n|[\r\n])$/))
        this.textbox.value += '\n';
    this.textbox.value += '----\n';
    this.onEdit();
}

Sporx.toggleEditMode = function() {
    this.deck.selectedIndex = (this.deck.selectedIndex == 0) ? 1 : 0;
}

Sporx.toggleEvaMode = function() {
    var check = document.getElementById('toggleEva');
    if (this.canvas.getAttribute('eva') == 'true') {
        this.canvas.removeAttribute('eva');
        check.checked = false;
    }
    else {
        this.canvas.setAttribute('eva', true);
        check.checked = true;
    }
}

Sporx.onPresentationClick = function(aEvent) {
    if (!this.isToolbarHidden)
        this.showHideToolbar();

    switch (aEvent.button) {
        case 0:
            var uri = aEvent.target.getAttribute('href');
            if (uri)
                window.open(uri);
            else {
                this.forward();
                document.documentElement.focus();
            }
            break;
        case 2:
            this.back();
            document.documentElement.focus();
            break;
        default:
            break;
    }
}

Sporx.onScrollerDragStart = function() {
    this.scroller.dragging = true;
}

Sporx.onScrollerDragMove = function() {
    if (this.scroller.dragging)
        this.showPage(parseInt(this.scroller.getAttribute('curpos')));
}

Sporx.onScrollerDragDrop = function() {
    if (this.scroller.dragging) {
        this.showPage(parseInt(this.scroller.getAttribute('curpos')));
    }
     this.scroller.dragging = false;
}

Sporx.onEdit = function() {
    this.data = Sporx.initializeData();
    this.takahashi();
}

Sporx.onKeyPress = function(aEvent) {
    switch (aEvent.keyCode) {
        case aEvent.DOM_VK_BACK_SPACE:
            if (this.isPresentationMode) {
                aEvent.preventBubble();
                aEvent.preventDefault();
                Sporx.back();
            }
            break;
        default:
            break;
    }
}

Sporx.onToolbarArea = false;
Sporx.toolbarHeight = 0;
Sporx.toolbarDelay = 300;
Sporx.toolbarTimer = null;
Sporx.isToolbarHidden = false;

Sporx.onMouseMoveOnCanvas = function(aEvent) {
    if (this.scroller.dragging) return;

    this.onToolbarArea = (aEvent.clientY < this.toolbarHeight);

    if (this.isToolbarHidden == this.onToolbarArea) {
        if (this.toolbarTimer)
            window.clearTimeout(this.toolbarTimer);

        this.toolbarTimer = window.setTimeout(
            'Sporx.onMouseMoveOnCanvasCallback()',
            this.toolbarDelay
        );
    }
}

Sporx.onMouseMoveOnCanvasCallback = function() {
    if (this.isToolbarHidden == this.onToolbarArea)
        this.showHideToolbar();
}

Sporx.toolbarAnimationDelay = 100;
Sporx.toolbarAnimationSteps = 5;
Sporx.toolbarAnimationInfo = null;
Sporx.toolbarAnimationTimer = null;

Sporx.showHideToolbar = function() {
    if (this.toolbarAnimationTimer)
        window.clearTimeout(this.toolbarAnimationTimer);

    this.toolbarAnimationInfo = { count : 0 };
    if (this.isToolbarHidden) {
        this.toolbarAnimationInfo.start = 0;
        this.toolbarAnimationInfo.end   = this.toolbarHeight;
    }
    else {
        this.toolbarAnimationInfo.start = this.toolbarHeight;
        this.toolbarAnimationInfo.end   = 0;
    }
    this.toolbarAnimationInfo.current = 0;

    this.toolbar.setAttribute(
        'style',
        'margin-top:' +
            (0-(this.toolbarHeight-this.toolbarAnimationInfo.start)) +
            'px; margin-bottom:' +
            (0-this.toolbarAnimationInfo.start) +
            'px;'
    );

    this.toolbarAnimationTimer = window.setTimeout(
        'Sporx.animateToolbar()',
        this.toolbarAnimationDelay/this.toolbarAnimationSteps
    );
}

Sporx.animateToolbar = function() {
    this.toolbarAnimationInfo.current +=
        parseInt(this.toolbarHeight/this.toolbarAnimationSteps);

    var top, bottom;
    if (this.toolbarAnimationInfo.start < this.toolbarAnimationInfo.end) {
        top    = this.toolbarHeight-this.toolbarAnimationInfo.current;
        bottom = this.toolbarAnimationInfo.current;
    }
    else {
        top    = this.toolbarAnimationInfo.current;
        bottom = this.toolbarHeight-this.toolbarAnimationInfo.current;
    }

    top    = Math.min(Math.max(top, 0), this.toolbarHeight);
    bottom = Math.min(Math.max(bottom, 0), this.toolbarHeight);

    this.toolbar.setAttribute(
        'style', 
        'margin-top:' + (0-top) + 'px; margin-bottom:' + (0-bottom) + 'px'
    );

    if (this.toolbarAnimationInfo.count < this.toolbarAnimationSteps) {
        this.toolbarAnimationInfo.count++;
        this.toolbarAnimationTimer = window.setTimeout(
            'Sporx.animateToolbar()',
            this.toolbarAnimationDelay/this.toolbarAnimationSteps
        );
    }
    else
        this.isToolbarHidden = !this.isToolbarHidden;
}

Sporx.readParameter = function() {
    if (location.hash && location.hash.match(/^#(\d+)$/)) {
        this.offset = parseInt(RegExp.$1) - 1;
    }
    if (location.search) {
        var param = location.search.replace(/^\?/, '');

        if (param.match(/page=([0-9]+)/i))
            this.offset = parseInt(RegExp.$1) - 1;

        if (param.match(/edit=(1|true|yes)/i))
            this.toggleEditMode();

        if (param.match(/eva=(1|true|yes)/i))
            this.toggleEvaMode();

        if (param.match(/data=([^&;]+)/i)) {
            var path = unescape(RegExp.$1);
            this.dataPath = path;
            if (location.href.match(/^https?:/)) {
                var request = new XMLHttpRequest();
                request.open('GET', path);
                request.onload = function() {
                    Sporx.textbox.value = request.responseText;
                    Sporx.data = Sporx.initializeData();

                    Sporx.takahashi();
                };
                request.send(null);
            }
            else {
                document.getElementById('dataLoader').
                    setAttribute('src', path);
            }
            return false;
        }
    }
    return true;
}

Sporx.onDataLoad = function() {
    if (!window.frames[0].document.body.hasChildNodes()) return;
    var data = window.frames[0].document.body.firstChild.innerHTML;
    if (!data) return;

    this.textbox.value = data;
    this.data = Sporx.initializeData();

    this.takahashi();
}

//------------------------------------------------------------------------------
// Debugging Support
//------------------------------------------------------------------------------

function XXX(msg) {
    if (! confirm(msg))
        throw("terminated...");
    return msg;
}

function JJJ(obj) {
    XXX(JSON.stringify(obj));
    return obj;
}

//------------------------------------------------------------------------------
// JSON Support
//------------------------------------------------------------------------------

/*
Copyright (c) 2005 JSON.org
*/
var JSON = function () {
    var m = {
            '\b': '\\b',
            '\t': '\\t',
            '\n': '\\n',
            '\f': '\\f',
            '\r': '\\r',
            '"' : '\\"',
            '\\': '\\\\'
        },
        s = {
            'boolean': function (x) {
                return String(x);
            },
            number: function (x) {
                return isFinite(x) ? String(x) : 'null';
            },
            string: function (x) {
                if (/["\\\x00-\x1f]/.test(x)) {
                    x = x.replace(/([\x00-\x1f\\"])/g, function(a, b) {
                        var c = m[b];
                        if (c) {
                            return c;
                        }
                        c = b.charCodeAt();
                        return '\\u00' +
                            Math.floor(c / 16).toString(16) +
                            (c % 16).toString(16);
                    });
                }
                return '"' + x + '"';
            },
            object: function (x) {
                if (x) {
                    var a = [], b, f, i, l, v;
                    if (x instanceof Array) {
                        a[0] = '[';
                        l = x.length;
                        for (i = 0; i < l; i += 1) {
                            v = x[i];
                            f = s[typeof v];
                            if (f) {
                                v = f(v);
                                if (typeof v == 'string') {
                                    if (b) {
                                        a[a.length] = ',';
                                    }
                                    a[a.length] = v;
                                    b = true;
                                }
                            }
                        }
                        a[a.length] = ']';
                    } else if (x instanceof Object) {
                        a[0] = '{';
                        for (i in x) {
                            v = x[i];
                            f = s[typeof v];
                            if (f) {
                                v = f(v);
                                if (typeof v == 'string') {
                                    if (b) {
                                        a[a.length] = ',';
                                    }
                                    a.push(s.string(i), ':', v);
                                    b = true;
                                }
                            }
                        }
                        a[a.length] = '}';
                    } else {
                        return;
                    }
                    return a.join('');
                }
                return 'null';
            }
        };
    return {
        copyright: '(c)2005 JSON.org',
        license: 'http://www.crockford.com/JSON/license.html',
        stringify: function (v) {
            var f = s[typeof v];
            if (f) {
                v = f(v);
                if (typeof v == 'string') {
                    return v;
                }
            }
            return null;
        },
        parse: function (text) {
            try {
                return !(/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test(
                        text.replace(/"(\\.|[^"\\])*"/g, ''))) &&
                    eval('(' + text + ')');
            } catch (e) {
                return false;
            }
        }
    };
}();
