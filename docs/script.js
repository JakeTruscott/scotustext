function openTab(tabNumber) {
  // Hide all content elements
  var contentElements = document.getElementsByClassName('content');
  for (var i = 0; i < contentElements.length; i++) {
    contentElements[i].style.display = 'none';
  }

  // Show the selected content element
  var selectedContent = document.getElementById('content' + tabNumber);
  if (selectedContent) {
    selectedContent.style.display = 'block';

    // Adjust iframe size to fill the entire page
    var iframe = selectedContent.querySelector('iframe');
    if (iframe) {
      iframe.style.width = '100%';
      iframe.style.height = '1000px'; // Adjust the height value as needed
    }
  }
}
