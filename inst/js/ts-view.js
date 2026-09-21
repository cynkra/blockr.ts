/*
 * blockr.ts -- the chart/table switch.
 *
 * One handler for the whole page. Each toggle carries the Shiny input it
 * writes in `data-input`, so a board with twenty previews needs twenty divs
 * and no extra wiring.
 *
 * MOUNT IS IDEMPOTENT. The toggle markup is part of the block's UI, which a
 * dock panel can tear down and rebuild, so `mount()` runs again on a div that
 * may already be wired. A flag on the element is cheaper than removing
 * listeners.
 */
(function () {
  'use strict';

  function wire(el) {
    if (el.dataset.tsViewWired === '1') return;
    el.dataset.tsViewWired = '1';

    el.addEventListener('click', function (ev) {
      var btn = ev.target.closest('.blockr-ts-view__btn');
      if (!btn || !el.contains(btn)) return;

      var view = btn.getAttribute('data-view');
      setActive(el, view);
      Shiny.setInputValue(el.getAttribute('data-input'), view,
                          {priority: 'event'});
    });
  }

  function setActive(el, view) {
    el.querySelectorAll('.blockr-ts-view__btn').forEach(function (b) {
      b.classList.toggle('is-active', b.getAttribute('data-view') === view);
    });
  }

  function mount() {
    document.querySelectorAll('.blockr-ts-view').forEach(wire);
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('blockr-ts-view', function (msg) {
      var el = document.getElementById(msg.id);
      if (!el) return;
      wire(el);
      // A block whose result is not drawable keeps the switch hidden, so a
      // board of ordinary data frames looks exactly as it did before this
      // display was turned on.
      el.classList.toggle('is-offered', !!msg.offer);
      setActive(el, msg.view || 'chart');
    });
  }

  // Called by the chart when it mounts: this block can draw, so it gets the
  // switch. Idempotent, and wires the element if it has not been wired yet.
  function offer(id) {
    var el = document.getElementById(id);
    if (!el) return;
    wire(el);
    el.classList.add('is-offered');
  }

  window.BlockrTsView = { mount: mount, offer: offer };
})();
