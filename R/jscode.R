jscode <- "shinyjs.disabletab =function(name){
$('ul li:has(a[data-value='+ name +'])').addClass('disabled');
$('.nav li.disabled a').prop('disabled',true)
}

shinyjs.enabletab =function(name){
$('.nav li.disabled a').prop('disabled',false)
$('ul li:has(a[data-value='+ name +'])').removeClass('disabled');
} "
