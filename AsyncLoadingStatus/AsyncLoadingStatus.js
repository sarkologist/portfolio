// the only name exported by this library
var AsyncLoadingStatus; // constructor (called without 'new')

(function(jQuery, global) {
  var $ = jQuery; // requires jQuery
  var numberOfInstances = 0;

  AsyncLoadingStatus = function(config) {
    var __callbacks = [];
    var isDone = false;
    var $defaultGif;
    var toPlaceGifOrNot = config && config.placeGifIn && config.baseURL;
    numberOfInstances += 1;
    var instanceNumber = numberOfInstances;

    // INITIALIZE ARGUMENTS FROM CONFIGURATION OBJECT

    if (toPlaceGifOrNot) {
      $defaultGif = $();

      // each field may have more than one handler registered; we don't want to add more than one gif per field
      $(config.placeGifIn).each(function() {
        var $this = $(this);
        var $possiblyExistingGif = $this.find('.ajax-loader');
        var $addedGif;

        if ($possiblyExistingGif.length > 0) {
          $defaultGif = $defaultGif.add($possiblyExistingGif);
        } else {
          $addedGif = $('<img>')
            .attr('class', 'ajax-loader')
            .attr('src', config.baseURL + '/profit/images/ajax-loader.gif')
            .attr('alt', 'checking...')
            .appendTo($this);

          $defaultGif = $defaultGif.add($addedGif);
        }
      });
    }

    // default argument values
    var defaults = {
          buttons: $('input[type=button]')  // default to working with all buttons on the page
                    .add('input[type=submit]')
                    .add('input[type=reset]')
                    // .not('input[type=reset]')
                    // .filter(function() {
                    //   var name = $(this).attr('name');

                    //   if (!name) return true;
                    //   else name = name.toLowerCase();

                    //   return name.indexOf('cancel') === -1 && name.indexOf('reset') === -1 ;
                    // });
        , loadingGif: toPlaceGifOrNot ? $defaultGif : $('img[src$="/profit/images/ajax-loader.gif"]') // default to working with all instances of this gif
        , callbacks: []
      };

    config = $.extend({}, defaults, config);

    // these arguments can accept whatever can be passed into the jQuery constructor; e.g. selector, DOM element, $ object, etc.
    var $loadingGif = $(config.loadingGif);
    buttons = $.map(config.buttons, function(button) {
      if (button === 'all_buttons')
        return $('input[type=button]').add('input[type=submit]').add('input[type=reset]');

      return $(button);
    });

    var handler = config.handler; // string - function name
    var callbacks = config.callbacks; // array of strings - each a function name
    if (typeof callbacks === 'string') { // if client passed in a string (to specify only one callback)
      callbacks = [callbacks];
    }

    // PRIVATE FUNCTIONS
    function disableButtons() {
      $.each(buttons, function(_, $button) {
        var list = $button.data('AsyncLoadingStatus_instances');
        if (!list) {
          $button.data('AsyncLoadingStatus_instances', [instanceNumber])
        } else if ($.inArray(instanceNumber, list) === -1) {
          list.push(instanceNumber);
          $button.data('AsyncLoadingStatus_instances', list)
        }
      });

      $.each(buttons, function(_, $button) {
        $button.attr("disabled", "disabled"); // jquery <1.6 does not support $.fn.prop
      });
    };

    function enableButtons() {
      $.each(buttons, function(_, $button) {
        var list = $button.data('AsyncLoadingStatus_instances');

        if (!list) {
          $button.removeAttr("disabled") // jquery <1.6 does not support $.fn.prop;
        } else {
          removeElem(list, instanceNumber);
          $button.data('AsyncLoadingStatus_instances', list)

          if (list.length === 0) {
            $button.removeAttr("disabled") // jquery <1.6 does not support $.fn.prop;
          }
        }
      });
    };

    function showGif() {
      if (!$loadingGif) return;

      // register this AsyncLoadingStatus instance on the gif
      $loadingGif.each(function() {
        var $this = $(this);
        var list = $this.data('AsyncLoadingStatus_instances');
        if (!list) {
          $this.data('AsyncLoadingStatus_instances', [instanceNumber])
        } else if ($.inArray(instanceNumber, list) === -1) {
          list.push(instanceNumber);
          $this.data('AsyncLoadingStatus_instances', list)
        }
      });

      $loadingGif && $loadingGif.show();
    }

    function hideGif() {
      if (!$loadingGif) return;

      $loadingGif.each(function() {
        var $this = $(this);
        var list = $this.data('AsyncLoadingStatus_instances');

        if (!list) {
          $this.hide();
        } else {
          removeElem(list, instanceNumber);
          $this.data('AsyncLoadingStatus_instances', list)

          if (list.length === 0) {
            $this.hide();
          }
        }
      });
    }

    // to be called upon call of handler
    // unhides loading gif; disables buttons
    function start() {
      if (isDone) {
        isDone = false;
        return;
      }

      if (__callbacks.length == 0) {
        throw new Error("AsyncLoadingStatus Error: no callbacks have been registered with the handler.");
        return;
      }

      $.each(__callbacks, function(_, callback) {
        callback._AsyncLoadingStatus_isWaiting = true;
      });

      showGif();
      disableButtons();

      setTimeout(timeout, 60000);
    };

    // to be called upon completion of callback
    // hides loading gif; re-enables buttons
    function end(f) {
      f._AsyncLoadingStatus_isWaiting = false;

      var sometrue = false;

      $.each(__callbacks, function(_, callback) {
        sometrue = sometrue || callback._AsyncLoadingStatus_isWaiting;
      });

      if (!sometrue) {
        hideGif();
        enableButtons();
      }
    };

    function getCallback(f, use_once) {
      if (typeof f !== 'function')
        throw new Error('AsyncLoadingStatus Error: expected function for callback');

      f._AsyncLoadingStatus_use_once = use_once;

      __callbacks.push(f); // note that we add a reference to f, not the modified callback

      return function() {
        f.apply(null, arguments); // call original function

        end(f);

        // remove from callback list if the callback is defined anew each time the handler is called
        if (use_once) {
          removeElem(__callbacks, f); // remove by comparing equality of reference to f
        }
      };
    };

    function getHandler(f) {
      if (typeof f !== 'function')
        throw new Error('AsyncLoadingStatus Error: expected function for handler');

      return function() {
        f.apply(null,arguments); // call original function
        start();
      }
    }

    function removeElem(array, elem) {
      for (var i=0; i < array.length; i++) {
        if (array[i] === elem)
          array.splice(i,1);
      }
    };

    function timeout() {
      hideGif();
      enableButtons();
    }

    // END PRIVATE FUNCTIONS

    // INITIALIZATION

    hideGif();

    // modify handlers
    if (handler) {
      if (handler.search(/^[A-Za-z_$]([\dA-Za-z_$])*$/) !== -1) { // check is valid identifier for js var name
        eval('global.' + handler + ' = getHandler(' + handler + ')');
      }
      else {
        throw new Error('AsyncLoadingStatus Error: invalid js identifier for handler: ' + handler);
      }
    }

    // modify callbacks
    $.each(callbacks, function(_, c) {
      if (c.search(/^[A-Za-z_$]([\dA-Za-z_$])*$/) !== -1) { // check is valid identifier for js var name
        eval('global.' + c + ' = getCallback(' + c + ')');
      }
      else {
        throw new Error('AsyncLoadingStatus Error: invalid js identifier for callback: ' + c);
      }
    });

    // EXPORTED METHODS

    var done = function() {
      isDone = true;
      $.each(__callbacks, function(_, c) {
        if (c._AsyncLoadingStatus_use_once) {
          removeElem(__callbacks, c);
        } else {
          c._AsyncLoadingStatus_isWaiting = false;
        }
      });

      hideGif();
      enableButtons();
    };

    return {
      getCallback: getCallback, // reveal
      getHandler: getHandler, // reveal
      done: done
    };

  };

})(jQuery, this); // we pass in 'this' because we need access to the scope where we can override the handler/callback functions