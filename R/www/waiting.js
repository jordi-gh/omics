setInterval(function(){
  if ($('html').attr('class')=='shiny-busy') {
      setTimeout(function() {
        if ($('html').attr('class')=='shiny-busy') {
            $('div.waiting').show()
        }
      }, 100)
  } else {
      $('div.waiting').hide()
  }
}, 100)