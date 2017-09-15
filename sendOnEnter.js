// This script just listens for "enter"s on the text input and simulates
// clicking the "send" button when that occurs. Totally optional.
// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".collection", function(e) {
  e.preventDefault();
  $el = $(this);
   var label = $el.data("label");
  Shiny.onInputChange("showEntity", {
    label: label,
    nonce: Math.random()
  });
});
$(document).on("click", ".corpus", function(e) {
  e.preventDefault();
  $el = $(this);
   var label = $el.data("label");
  Shiny.onInputChange("showNote", {
    label: label,
    nonce: Math.random()
  });
});
$(document).on("click", ".cloud", function(e) {
  e.preventDefault();
  $el = $(this);
   var label = $el.data("label");
  Shiny.onInputChange("showTag", {
    label: label,
    nonce: Math.random()
  });
});
jQuery(document).ready(function(){
  var elementTags = ace.edit("annotationBody");
elementTags.commands.bindKey("Tab",null);
elementTags.commands.bindKey("Shift+Tab",null);
    }
);

jQuery(document).ready(function(){
  jQuery('#entry').keypress(function(evt){
    if (evt.keyCode == 13){
      // Enter, simulate clicking send
      jQuery('#send').click();
    }
  });
})


// We don't yet have an API to know when an element is updated, so we'll poll
// and if we find the content has changed, we'll scroll down to show the new
// comments.
var oldContent = null;
window.setInterval(function() {
var elem = document.getElementById('chat');
if (oldContent != elem.innerHTML){
scrollToBottom();
}
oldContent = elem.innerHTML;
}, 300);
// Scroll to the bottom of the chat window.
function scrollToBottom(){
var elem = document.getElementById('chat');
elem.scrollTop = elem.scrollHeight;
}
