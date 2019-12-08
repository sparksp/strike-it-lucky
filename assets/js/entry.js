import "../sass/style.scss";
import { Elm } from "../../src/Main.elm";

document.addEventListener("DOMContentLoaded", function() {
    const mountPoint = document.querySelector("main");
    Elm.Main.init({
        node: mountPoint,
        flags: null
    });
});
