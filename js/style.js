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
      var xs = document.getElementsByClassName('sourceCode');
      sourceCode = [];

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
