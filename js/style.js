(function() {
  resizeSourceCode();
  window.onresize = resizeSourceCode;

  var sourceCode = null;
  function resizeSourceCode()
  {
    if (sourceCode == null) {
      var xs = document.getElementsByClassName('sourceCode');
      if (!xs.length) return;

      sourceCode = [];
      for (var i = 0; i < xs.length; i++) {
        if (xs[i].parentNode.nodeName == 'BODY') {
          sourceCode.push(xs[i]);
        }
      }
    }

    if (!sourceCode.length) return;

    var sample = sourceCode[0];
    sample.style.display = 'block';
    sample.style.marginLeft = '0';
    sample.style.paddingLeft = '0';
    sample.style.marginRight = '0';

    var docWidth = document.documentElement.clientWidth;
    var preWidth = sample.offsetWidth;

    var marginWidth = (docWidth - preWidth) / 2;
    if (marginWidth < 0)
      marginWidth = 0;

    for (var i = 0; i < sourceCode.length; i++) {
      sourceCode[i].style.display = 'block';
      sourceCode[i].style.marginLeft = '-' + marginWidth + 'px';
      sourceCode[i].style.marginRight = '-' + marginWidth + 'px';
      sourceCode[i].style.paddingLeft = marginWidth + 'px';
    }
  }
})();
