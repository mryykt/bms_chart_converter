import "./style.css";
import * as bms from "bms";
import { Elm } from "./Main.elm";

let app = Elm.Main.init({ node: document.querySelector("main") });

app.ports.loadBMS.subscribe(async (file: { name: string; buf: string }) => {
  console.log(bms.Compiler.compile(file.buf));
});
