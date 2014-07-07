/* --------------------------------------------------------------------------
 * General JavaScript settings for the application `Smap`.
 *
 * Author: Lasse Kristopher Meyer
 * Version: January 2014
 * -------------------------------------------------------------------------- */

/* --------------------------------------------------------------------------
 * -- Global variables and actions on page load                            --
 * -------------------------------------------------------------------------- */

// Holds the initial value of the editor source code field (if in DOM).
var editorCodeInitial;

// Actions be executed when the DOM is fully loaded.
$(document).ready(function() {
  // show alert dialog box on page load (if available)
  $('#alert-error-dialog-box').modal('show');
  $('#alert-info-dialog-box').modal('show');
  $('#alert-success-dialog-box').modal('show');
  $('#alert-warning-dialog-box').modal('show');

  // get the initial value of the editor source code field for future resets
  // with #editor-reset-btn (see editor section below)
  var editorCode = $('#editor');
  // check if element exists in the DOM
  if ('#editor'.length) {
    editorCodeInitial = editorCode.val();
  }

  // set search panel target checkboxes depending on the input field state 
  // (should only be enabled if the input field is not empty)
  var keyword = $('#search-panel-keyword-input');
  // check if element exists in the DOM
  if (keyword.length) {
    if (keyword.val().length > 0) {
      enableSearchPanelCheckboxes();
    } else {
      disableSearchPanelCheckboxes();
    }
  }
});

/* --------------------------------------------------------------------------
 * -- Navbar search and search panel                                       --
 * -------------------------------------------------------------------------- */

/* -- Navbar search --------------------------------------------------------- */

// Get all navbar search components.
var nsKeywordInput = $('#navbar-search-keyword-input');
var nsApplyButton  = $('#navbar-search-apply-button');

// Reads the keyword from the input field and builds the URL.
nsApplyButton.click(function() {

  // get value
  var keyword = $.trim(nsKeywordInput.val());

  // build query string
  var queryString = '';
  if (keyword.length > 0) {
    queryString = '?q='+keyword
  } else {
    return false;
  }

  // build URL
  var baseUrl = nsApplyButton.attr('href');
  nsApplyButton.attr('href',baseUrl+queryString);
});

// Apply search if keyword input field is focused and enter is pressed.
nsKeywordInput.keydown(function(e) { 
  if (e.keyCode == 13) {
    e.preventDefault();
    nsApplyButton[0].click();
  }
});

/* -- Search panel ---------------------------------------------------------- */

// Get all search panel components.
var spKeywordInput  = $('#search-panel-keyword-input');
var spTitleCheckbox = $('#search-panel-title-checkbox');
var spDescrCheckbox = $('#search-panel-descr-checkbox');
var spTagsCheckbox  = $('#search-panel-tags-checkbox');
var spLangSelect    = $('#search-panel-lang-select');
var spSortSelect    = $('#search-panel-sort-select');
var spOrderSelect   = $('#search-panel-order-select');
var spApplyButton   = $('#search-panel-apply-button');
var spResetButton   = $('#search-panel-reset-button');

// Enables search panel checkboxes if the input field is not empty and disables
// them otherwise.
spKeywordInput.on("keyup",function() {
  if (spKeywordInput.val().length > 0) {
    enableSearchPanelCheckboxes();
  } else {
    disableSearchPanelCheckboxes();
  }
});

// Checks all search panel checkboxes.
function checkSearchPanelCheckboxes() {
  spTitleCheckbox.prop('checked',true);
  spDescrCheckbox.prop('checked',true);
  spTagsCheckbox.prop('checked',true);
};

// Enables all search panel checkboxes.
function enableSearchPanelCheckboxes() {
  spTitleCheckbox.prop('disabled',false);
  spDescrCheckbox.prop('disabled',false);
  spTagsCheckbox.prop('disabled',false);
};

// Disables all search panel checkboxes.
function disableSearchPanelCheckboxes() {
  spTitleCheckbox.prop('disabled',true);
  spDescrCheckbox.prop('disabled',true);
  spTagsCheckbox.prop('disabled',true);
};

// Applies all search settings and builds the corresponding query string.
spApplyButton.click(function() {

  // get values
  var keyword = spKeywordInput.val();
  var lang    = spLangSelect.val();
  var sort    = spSortSelect.val();
  var order   = spOrderSelect.val();

  // build query string
  var queryString = [];
  keyword = encodeURIComponent(keyword.trim());
  if (keyword.length > 0) {
    var targets = [];
    if (spTitleCheckbox.is(':checked')) {
      targets.push('title');
    }
    if (spDescrCheckbox.is(':checked')) {
      targets.push('descr');
    }
    if (spTagsCheckbox.is(':checked')) {
      targets.push('tags');
    }
    targets.toString();
    queryString.push('q='+keyword);
    queryString.push('targets='+targets);
  }
  if (lang.length > 0) {
    queryString.push('lang='+lang);
  }
  queryString.push('sort='+sort);
  queryString.push('order='+order);

  // build URL
  if (queryString.length > 0) {
    var baseUrl = spApplyButton.attr('href');
    queryString = queryString.join('&');
    spApplyButton.attr('href',baseUrl+'?'+queryString);
  }
});

// Resets all search panel components to their default value.
spResetButton.click(function() {
  spKeywordInput.val('');
  checkSearchPanelCheckboxes();
  disableSearchPanelCheckboxes();
  spLangSelect[0].selectedIndex = 0;
  spSortSelect[0].selectedIndex = 0;
  spOrderSelect[0].selectedIndex = 0;
  return false;
});

// Apply search if keyword input field is focused and enter is pressed.
spKeywordInput.keydown(function(e) { 
  if (e.keyCode == 13) {
    e.preventDefault();
    spApplyButton[0].click();
  }
});

/* --------------------------------------------------------------------------
 * -- Navbar fork form                                                     --
 * -------------------------------------------------------------------------- */

// Prevent dropdown menu from hiding if the form is clicked.
$('.dropdown-toggle').dropdown();
$('.dropdown-menu').find('#navbar-fork-form').click(function(e) {
  e.stopPropagation();
});

// Get all navbar fork form components.
var nffIdInput     = $('#navbar-fork-id-input');
var nffApplyButton = $('#navbar-fork-apply-button');

// Reads the program ID from the input field and builds the URL.
nffApplyButton.click(function() {

  // get ID
  var id = $.trim(nffIdInput.val());

  // build URL
  if (id.length > 0) {
    var baseUrl = nffApplyButton.attr('href');
    nffApplyButton.attr('href',baseUrl+id);
  } else {
    return false;
  }
})

// Apply fork if keyword input field is focused and enter is pressed.
nffIdInput.keydown(function(e) { 
  if (e.keyCode == 13) {
    e.preventDefault();
    nffApplyButton[0].click();
  }
});

/* --------------------------------------------------------------------------
 * -- Tooltips                                                             --
 * -------------------------------------------------------------------------- */

// Tooltip initialisation.
$(function() {
  // error tooltips in WUI forms
  $(".with-error").tooltip({
      container: 'body',
      placement: 'right',
      trigger: 'manual'
  });
  $(".with-error").tooltip('show');

  // IE buttons
  $('#run-button').tooltip({
    container: 'body',
    placement: 'top'
  });
  $('#reset-button').tooltip({
    container: 'body',
    placement: 'top',
  });
  $('#clear-button').tooltip({
    container: 'body',
    placement: 'top'
  });

  // quick search bar tooltip
  $('#navbar-search-form').tooltip({
    container: 'body',
    placement: 'bottom'
  });
});

/* --------------------------------------------------------------------------
 * -- Comment form                                                         --
 * -------------------------------------------------------------------------- */

// Get all comment form components.
var commentTextInput    = $('#comment-text-input');
var commentSubmitButton = $('#comment-submit-button');

// Enables the comment submit button if the input field is not empty and
// disables it otherwise.
commentTextInput.on("keyup",function() {
  if (commentTextInput.val().length > 0) {
    commentSubmitButton.attr("disabled",false);
  } else {
    commentSubmitButton.attr("disabled",true);
  }
});

/* --------------------------------------------------------------------------
 * -- SmapIE drowdown ID input                                             --
 * -------------------------------------------------------------------------- */

// Prevents the dropdown menu from hiding when focussing the input field.
$('.dropdown-toggle').dropdown();
$('.dropdown-menu').find('#navbar-fork-form').click(function (e) {
  e.stopPropagation();
});

/* --------------------------------------------------------------------------
 * -- SmapIE button functionality                                          --
 * -------------------------------------------------------------------------- */

$('#reset-button').click(function() {
  code.setValue(editorCodeInitial);
});

$('#clear-button').click(function() {
  code.setValue('');
});

$('#version-select').change(function() {
  var value = $('#version-select').val();
  window.location.href = value;
});

/* --------------------------------------------------------------------------
 * -- Pagination elements                                                  --
 * -------------------------------------------------------------------------- */

$('.previous.disabled').click(function() {
  return false;
});

$('.next.disabled').click(function() {
  return false;
});

/* -------------------------------------------------------------------------- */