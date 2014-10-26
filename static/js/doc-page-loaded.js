AJS.$(function() {
   // Make it so that the markdown content is converted into HTML content
   AJS.$("#page-html-content").html(marked(AJS.$("#page-markdown-content").text()));

   // Make it so that the items that link to the current page in the side nav are highlighted
   var currentPath = window.location.pathname;
      AJS.$(".aui-nav li").each(function(i, link) {
      var listItem = AJS.$(link);
      if(listItem.find("a").attr("href") == currentPath) {
         listItem.addClass("aui-nav-selected");
      }
   });
});
