// Function to open the selected tab and load content
function openPage(pageName) {
  // Get all tab content elements
  var tabContent = document.getElementsByClassName("tabcontent");
  // Hide all tab content
  for (var i = 0; i < tabContent.length; i++) {
    tabContent[i].style.display = "none";
  }

  // Show the selected tab content
  document.getElementById(pageName).style.display = "block";
}

// Set the initial tab
openPage('home');
