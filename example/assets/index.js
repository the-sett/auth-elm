const {
  Elm
} = require('../src/elm/Main.elm');

const app = Elm.Main.init({
  node: document.getElementById('diagram')
});
