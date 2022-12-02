$( document ).ready(function() {
  $('#recent_radar-generate_chart').on("click", ".treeview > a", function() {
  var $menu = $(this).next();
  // If this menuItem was already open, then clicking on it again,
  // should trigger the "hidden" event, so Shiny doesn't worry about
  // it while it's hidden (and vice versa).
  if ($menu.hasClass("menu-open")) $menu.trigger("hidden");
  //else if ($menu.hasClass("treeview-menu")) $menu.trigger("shown");

  // need to set timeout to account for the slideUp/slideDown animation
  var $obj = $('section.sidebar.shiny-bound-input');
  setTimeout(function() { $obj.trigger('change'); }, 600);
});

$("#recent_radar-generate_chart").click(function(){
  $('#sidebarCollapsed').attr( "data-collapsed", true);
  $('body').addClass('sidebar-collapse')
});

// console.log(111)


});


