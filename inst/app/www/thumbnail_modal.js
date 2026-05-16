$(document).ready(function() {
  const elem = document.getElementById('modal-thumbnail-body');
  const panzoom = Panzoom(elem, {
    maxScale: 5,
    minScale: 1,
    bounds: true,
    boundsPadding: 0,
    disablePan: false,
  });

  $('#thumbnailModal').dialog({
    autoOpen: false,
    modal: true,
    draggable: false,
    resizable: false,
    width: Math.min(window.innerWidth * 0.9, 1000),
    maxHeight: Math.floor(window.innerHeight * 0.9),
    position: { my: 'center center', at: 'center center', of: window },  //Center the dialog
    closeOnEscape: true,
    open: function() {
      const currentWidth = Math.min(window.innerWidth * 0.9, 1000);
      const maxModalHeight = Math.floor(window.innerHeight * 0.9);
      
      $('#thumbnailModal').dialog('option', {
        'width': currentWidth,
        'maxHeight': maxModalHeight
      });
      
      const captionHeight = $('.modal-thumbnail-caption').outerHeight() || 150;
      const controlsHeight = 60;
      const maxImageHeight = maxModalHeight - captionHeight - controlsHeight - 40;
      
      $('#modalThumbnailImage').css({
        'max-width': '100%',
        'max-height': maxImageHeight + 'px',
        'width': 'auto',
        'height': 'auto',
        'display': 'block',
        'margin': '0 auto'
      });
      
      $('#thumbnailModal').css({
        'height': 'auto',
        'max-height': maxModalHeight + 'px',
        'min-height': '200px',
        'overflow': 'auto'
      });
      
      const baseFontSize = Math.max(12, Math.min(16, window.innerWidth / 100));
      $('.modal-thumbnail-caption').css({
        'font-size': baseFontSize + 'px',
        'line-height': '1.4',
        'max-height': captionHeight + 'px',
      });

      if ($('.zoom-controls').length === 0) {
        $('#thumbnailModal').append(`
          <div class="zoom-controls">
            <button type="button" id="zoomIn" class="btn btn-secondary" title="Zoom-in">&plus;</button>
            <button type="button" id="zoomOut" class="btn btn-secondary" title="Zoom-out">&minus;</button>
            <button type="button" id="reset" class="btn btn-secondary" title="Reset image">&#8634;</button>
          </div>
        `);
      }

      $('#zoomIn').on('click', function() {
        panzoom.zoomIn();
      });

      $('#zoomOut').on('click', function() {
        panzoom.zoomOut();
      });

      $('#reset').on('click', function() {
        panzoom.reset();
      });

      const $dialog = $('#thumbnailModal').dialog('widget');
      const dialogHeight = $dialog.outerHeight();
      const windowHeight = window.innerHeight;
      const topPosition = Math.max(0, (windowHeight - dialogHeight) / 2);
      
      $dialog.css({
        'top': topPosition + 'px',
        'position': 'fixed',
        'z-index': '5000'
      });
    },

    close: function() {
      $(this).dialog('close');
      panzoom.reset();
      $('.zoom-controls').remove();
    }
  });

  $('#thumbnail_image').on('click', function() {
    $('#thumbnailModal').dialog('open');
  });

  //Window resize to keep dialog responsive
  $(window).on('resize', function() {
    if ($('#thumbnailModal').dialog('isOpen')) {
      const newWidth = Math.min(window.innerWidth * 0.9, 1000);
      const maxModalHeight = Math.floor(window.innerHeight * 0.9);
      
      const captionHeight = $('.modal-thumbnail-caption').outerHeight() || 150;
      const controlsHeight = 60;
      const maxImageHeight = maxModalHeight - captionHeight - controlsHeight - 40;
      
      $('#thumbnailModal').dialog('option', {
        'width': newWidth,
        'maxHeight': maxModalHeight,
        'position': { my: 'center center', at: 'center center', of: window }
      });
      
      $('#modalThumbnailImage').css({
        'max-height': maxImageHeight + 'px'
      });

      const baseFontSize = Math.max(10, Math.min(14, window.innerWidth / 100));
      $('.modal-thumbnail-caption').css({
        'font-size': baseFontSize + 'px'
      });
      
      $('#thumbnailModal').css({
        'height': 'auto',
        'max-height': maxModalHeight + 'px'
      });
    }
  });
});
