

$(document).ready(function(){

// Set up Autocomplete
function makeBloodhoundFor(options) {
  var resource = options.resource;
  var field = options.field;

  return new Bloodhound({
    datumTokenizer: function(datum) {
      var retval = Bloodhound.tokenizers.whitespace(datum[field]);
      return retval;
    },
    queryTokenizer: Bloodhound.tokenizers.whitespace,
    prefetch: {
      url: 'http://localhost:8080/' + resource + '/prefetch'
    }
  });
}

function setTypeaheadOptionsFor(options) {
  var resourceName = options.resource;
  var displayName = options.displayName;
  var field = options.field;
  var bloodhound = makeBloodhoundFor(options);

  return {
    name: resourceName,
    display: function (input) {
      return input[field];
    },
    templates: {
      header: '<strong>' + displayName + '</strong>',
      suggestion: Handlebars.compile('<div class="suggestion"><strong><a href="http://localhost:8080/' + resourceName + '/{{id}}">{{' + field +'}}</a></strong></div>')
    },
    source: bloodhound 
  };
}

var showOptions = {
  resource: 'shows',
  field: 'title',
  displayName: 'Productions'
};
var orgOptions = {
  resource: 'organizations',
  field: 'name',
  displayName: 'Organizations'
};
var peopleOptions = {
  resource: 'people',
  field: 'name',
  displayName: 'People'
};
var workOptions = {
  resource: 'works',
  field: 'title',
  displayName: 'Pieces'
};

var shows = setTypeaheadOptionsFor(showOptions);
var orgs = setTypeaheadOptionsFor(orgOptions);
var people = setTypeaheadOptionsFor(peopleOptions);
var works = setTypeaheadOptionsFor(workOptions);

$('.searchdiv .typeahead').typeahead(null, shows, orgs, people, works);

});
