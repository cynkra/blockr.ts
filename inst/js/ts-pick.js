/*
 * blockr.ts -- the series picker.
 *
 * Everything here is local except the one thing the server needs: the array
 * of picked ids. Filtering and sorting never leave the browser, because
 * round-tripping a keystroke to re-render 168 rows is a keystroke you feel.
 *
 * THE TICK MARKS ARE OWNED HERE, NOT BY R. The R side renders the list when
 * the DATA changes and never when the selection does, so a click paints
 * immediately and the value R holds is whatever was last sent. Re-rendering
 * from R's own echo would move the list under the pointer between one click
 * and the next.
 */
(function () {
  'use strict';

  function mount(id) {
    var root = document.getElementById(id);
    if (!root || root._tsPick) return;
    root._tsPick = true;

    var inputId = root.getAttribute('data-input');
    var list = root.querySelector('.blockr-ts-pick__list');
    var rows = Array.prototype.slice.call(
      root.querySelectorAll('.blockr-ts-pick__row'));
    var count = root.querySelector('.blockr-ts-pick__count');
    var q = root.querySelector('.blockr-ts-pick__q');
    var sort = root.querySelector('.blockr-ts-pick__sort');
    var clear = root.querySelector('.blockr-ts-pick__link');

    function picked() {
      return rows.filter(function (r) {
        return r.classList.contains('is-on');
      }).map(function (r) { return r.dataset.id; });
    }

    function send() {
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue(inputId, picked(), {priority: 'event'});
      }
    }

    function paintCount() {
      var n = picked().length;
      var shown = rows.filter(function (r) {
        return !r.classList.contains('is-hidden');
      }).length;
      count.textContent = n + ' of ' + rows.length + ' selected' +
        (shown < rows.length ? ', ' + shown + ' shown' : '');
    }

    // LISTEN FOR `change`, NOT `click`. The row is a <label> around the
    // checkbox, so a click on the row activates the box and the box then
    // fires its own click that bubbles back here: two events for one
    // gesture, and a handler that flips the box itself flips it twice.
    // `change` fires once, after the browser has already done the toggle.
    root.addEventListener('change', function (ev) {
      var box = ev.target;
      if (!box.matches || !box.matches('input[type=checkbox]')) return;
      var row = box.closest('.blockr-ts-pick__row');
      if (!row) return;
      row.classList.toggle('is-on', box.checked);
      paintCount();
      send();
    });

    q.addEventListener('input', function () {
      var needle = q.value.toLowerCase().trim();
      rows.forEach(function (r) {
        r.classList.toggle('is-hidden',
          needle.length > 0 && r.dataset.q.indexOf(needle) === -1);
      });
      paintCount();
    });

    sort.addEventListener('change', function () {
      var by = sort.value;
      var out = rows.slice();
      if (by === 'last') {
        out.sort(function (a, b) { return b.dataset.last - a.dataset.last; });
      } else if (by === 'obs') {
        out.sort(function (a, b) { return b.dataset.obs - a.dataset.obs; });
      }
      out.forEach(function (r) { list.appendChild(r); });
    });

    clear.addEventListener('click', function () {
      rows.forEach(function (r) {
        r.classList.remove('is-on');
        r.querySelector('input[type=checkbox]').checked = false;
      });
      paintCount();
      send();
    });

    paintCount();
  }

  window.BlockrTsPick = { mount: mount };
})();
