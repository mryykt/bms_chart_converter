import "./style.scss";
import * as bms from "bms";
import { Elm } from "./Main.elm";

let app = Elm.Main.init({ node: document.querySelector("main") });

app.ports.compileBMS.subscribe(async (file: { name: string; buf: string }) => {
  let b = bms.Compiler.compile(file.buf);
  app.ports.loadBMS.send({
    name: file.name,
    header: (b.chart.headers as any)._dataAll,
    mlens: (b.chart.timeSignatures as any)._values,
    data: b.chart.objects.all(),
  });
});
