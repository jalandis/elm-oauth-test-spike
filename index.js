import { Elm } from './src/Main.elm';

const app = Elm.Main.init({});

app.ports.setLocalStorageItem.subscribe(function(data) {
  localStorage.setItem(data.key, data.value);
});
