// getElementsByClassName: developed by Robert Nyman, http://www.robertnyman.com
// Code/licensing: http://code.google.com/p/getelementsbyclassname/
var getElementsByClassName = function (className, tag, elm) {
  if (document.getElementsByClassName) {
    getElementsByClassName = function (className, tag, elm) {
      elm = elm || document;
      var elements = elm.getElementsByClassName(className),
          nodeName = (tag)? new RegExp("\\b" + tag + "\\b", "i") : null,
          returnElements = [],
          current;
      for(var i=0, il=elements.length; i<il; i+=1){
        current = elements[i];
        if(!nodeName || nodeName.test(current.nodeName)) {
          returnElements.push(current);
        }
      }
      return returnElements;
    };
  } else if (document.evaluate) {
    getElementsByClassName = function (className, tag, elm) {
      tag = tag || "*";
      elm = elm || document;
      var classes = className.split(" "),
          classesToCheck = "",
          xhtmlNamespace = "http://www.w3.org/1999/xhtml",
          namespaceResolver = (document.documentElement.namespaceURI === xhtmlNamespace)? xhtmlNamespace : null,
          returnElements = [],
          elements,
          node;
      for(var j=0, jl=classes.length; j<jl; j+=1){
        classesToCheck += "[contains(concat(' ', @class, ' '), ' " + classes[j] + " ')]";
      }
      try	{
        elements = document.evaluate(".//" + tag + classesToCheck, elm, namespaceResolver, 0, null);
      } catch (e) {
        elements = document.evaluate(".//" + tag + classesToCheck, elm, null, 0, null);
      }
      while ((node = elements.iterateNext())) {
        returnElements.push(node);
      }
      return returnElements;
    };
  } else {
    getElementsByClassName = function (className, tag, elm) {
      tag = tag || "*";
      elm = elm || document;
      var classes = className.split(" "),
          classesToCheck = [],
          elements = (tag === "*" && elm.all)? elm.all : elm.getElementsByTagName(tag),
          current,
          returnElements = [],
          match;
      for(var k=0, kl=classes.length; k<kl; k+=1){
        classesToCheck.push(new RegExp("(^|\\s)" + classes[k] + "(\\s|$)"));
      }
      for(var l=0, ll=elements.length; l<ll; l+=1){
        current = elements[l];
        match = false;
        for(var m=0, ml=classesToCheck.length; m<ml; m+=1){
          match = classesToCheck[m].test(current.className);
          if (!match) {
            break;
          }
        }
        if (match) {
          returnElements.push(current);
        }
      }
      return returnElements;
    };
  }
  return getElementsByClassName(className, tag, elm);
};

// Code for resizing source code boxes
// Original idea from: http://www.quirksmode.org
(function() {
  resizeSourceCode();
  window.onload = resizeSourceCode;
  window.onresize = resizeSourceCode;

  var sourceCode = null;
  function resizeSourceCode()
  {
    if (sourceCode == null) {
      sourceCode = [];

      var xs = getElementsByClassName('sourceCode');
      if (!xs.length) return;

      for (var i = 0; i < xs.length; i++) {
        if (xs[i].parentNode.nodeName == 'BODY') {
          sourceCode.push(xs[i]);
        }
      }

      // source code block are hidden by default, show them
      for (var i = 0; i < sourceCode.length; i++) {
        sourceCode[i].style.display = 'block';
      }
    }

    if (!sourceCode.length) return;

    // take the first source code block on the page
    // as a representative of all the others.
    var block      = sourceCode[0];
    var marginLeft = block.style.marginLeft;

    // set the margins to zero so we can take some
    // measurements
    block.style.marginLeft  = '0';
    block.style.marginRight = '0';

    // measure the source code block relative to
    // the rest of the page
    var pageWidth = document.documentElement.clientWidth;
    var sourceWidth = block.offsetWidth;
    var marginWidth = (pageWidth - sourceWidth) / 2;
    if (marginWidth < 0)
      marginWidth = 0;

    // propagate the correct margins to every source
    // code block on the page
    for (var i = 0; i < sourceCode.length; i++) {
      sourceCode[i].style.marginLeft  = marginLeft;
      sourceCode[i].style.marginRight = '-' + marginWidth + 'px';
    }
  }
})();

/*
CSS Browser Selector v0.4.0 (Nov 02, 2010)
Rafael Lima (http://rafael.adm.br)
http://rafael.adm.br/css_browser_selector
License: http://creativecommons.org/licenses/by/2.5/
Contributors: http://rafael.adm.br/css_browser_selector#contributors
*/
function css_browser_selector(u){var ua=u.toLowerCase(),is=function(t){return ua.indexOf(t)>-1},g='gecko',w='webkit',s='safari',o='opera',m='mobile',h=document.documentElement,b=[(!(/opera|webtv/i.test(ua))&&/msie\s(\d)/.test(ua))?('ie ie'+RegExp.$1):is('firefox/2')?g+' ff2':is('firefox/3.5')?g+' ff3 ff3_5':is('firefox/3.6')?g+' ff3 ff3_6':is('firefox/3')?g+' ff3':is('gecko/')?g:is('opera')?o+(/version\/(\d+)/.test(ua)?' '+o+RegExp.$1:(/opera(\s|\/)(\d+)/.test(ua)?' '+o+RegExp.$2:'')):is('konqueror')?'konqueror':is('blackberry')?m+' blackberry':is('android')?m+' android':is('chrome')?w+' chrome':is('iron')?w+' iron':is('applewebkit/')?w+' '+s+(/version\/(\d+)/.test(ua)?' '+s+RegExp.$1:''):is('mozilla/')?g:'',is('j2me')?m+' j2me':is('iphone')?m+' iphone':is('ipod')?m+' ipod':is('ipad')?m+' ipad':is('mac')?'mac':is('darwin')?'mac':is('webtv')?'webtv':is('win')?'win'+(is('windows nt 6.0')?' vista':''):is('freebsd')?'freebsd':(is('x11')||is('linux'))?'linux':'','js']; c = b.join(' '); h.className += ' '+c; return c;}; css_browser_selector(navigator.userAgent);
