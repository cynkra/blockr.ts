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
    var body = root.querySelector('tbody');
    var rows = Array.prototype.slice.call(
      root.querySelectorAll('.blockr-ts-pick__row'));
    var count = root.querySelector('.blockr-ts-pick__count');
    var q = root.querySelector('.blockr-search');
    var allCell = root.querySelector('.blockr-ts-pick__all');
    var allBox = allCell ? allCell.querySelector('input') : null;
    var sort = {key: null, dir: 1};

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

    function visible() {
      return rows.filter(function (r) {
        return !r.classList.contains('is-hidden');
      });
    }

    function paintCount() {
      var n = picked().length;
      var shown = visible();
      count.textContent = n + ' of ' + rows.length + ' selected' +
        (shown.length < rows.length ? ', ' + shown.length + ' shown' : '');

      // Tri-state, over the rows the filter is showing: checked when they
      // are all picked, indeterminate while some are.
      if (!allBox) return;
      var on = shown.filter(function (r) {
        return r.classList.contains('is-on');
      }).length;
      allBox.checked = shown.length > 0 && on === shown.length;
      allBox.indeterminate = on > 0 && on < shown.length;
    }

    function setRow(row, on) {
      row.classList.toggle('is-on', on);
      var box = row.querySelector('input[type=checkbox]');
      if (box) box.checked = on;
    }

    // ONE EVENT PER GESTURE. The checkbox is `pointer-events: none` and the
    // row carries the click, so there is no label forwarding a second
    // activation to the box and no visible double toggle.
    root.addEventListener('click', function (ev) {
      var row = ev.target.closest('.blockr-ts-pick__row');
      if (!row || !root.contains(row)) return;
      setRow(row, !row.classList.contains('is-on'));
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

    /* ---- sorting, on the column header ---------------------------------
       Where a reader of a table looks for it. First click sorts, second
       reverses, and the arrow says which. Text sorts up, numbers sort down
       first, because "which is the biggest" is the question you ask of a
       number column and "where is X" of a name column. */
    var NUMERIC = {obs: true, last: true};

    function applySort() {
      if (!sort.key) return;
      var key = sort.key;
      var num = NUMERIC[key];
      var out = rows.slice().sort(function (a, b) {
        var x = a.dataset[key];
        var y = b.dataset[key];
        if (num) return (x - y) * sort.dir;
        return x.localeCompare(y) * sort.dir;
      });
      // One reflow, not 168: the rows are moved inside a fragment first.
      var frag = document.createDocumentFragment();
      out.forEach(function (r) { frag.appendChild(r); });
      body.appendChild(frag);
    }

    function paintIcons() {
      root.querySelectorAll('.blockr-sortable').forEach(function (th) {
        var icon = th.querySelector('.blockr-sort-icon');
        var on = th.dataset.sort === sort.key;
        if (icon) {
          icon.className = 'blockr-sort-icon' + (on
            ? (sort.dir > 0 ? ' blockr-sort-icon-asc' : ' blockr-sort-icon-desc')
            : '');
        }
        th.setAttribute('aria-sort', on
          ? (sort.dir > 0 ? 'ascending' : 'descending') : 'none');
      });
    }

    root.querySelectorAll('.blockr-sortable').forEach(function (th) {
      th.addEventListener('click', function () {
        var key = th.dataset.sort;
        if (sort.key === key) {
          sort.dir = -sort.dir;
        } else {
          sort.key = key;
          sort.dir = NUMERIC[key] ? -1 : 1;
        }
        applySort();
        paintIcons();
      });
    });

    if (allCell) {
      allCell.addEventListener('click', function () {
        var shown = visible();
        var all = shown.length > 0 && shown.every(function (r) {
          return r.classList.contains('is-on');
        });
        shown.forEach(function (r) { setRow(r, !all); });
        paintCount();
        send();
      });
    }

    paintCount();
    paintIcons();
  }

  window.BlockrTsPick = { mount: mount };
})();
