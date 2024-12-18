import "./style.css";
import * as bms from "bms";
import { Elm } from "./Main.elm";

let app = Elm.Main.init({ node: document.querySelector("main") });

app.ports.compileBMS.subscribe(async (file: { name: string; buf: string }) => {
  let b = bms.Compiler.compile(file.buf);
  let waves: { [id: string]: string } = {};
  b.chart.headers.each((k, v) => {
    if (k.startsWith("wav")) {
      waves[k.slice(3)] = v;
    }
  });
  const bpm = b.chart.headers.get("bpm");
  const lnobj = b.chart.headers.get("lnobj");
  if (bpm) {
    app.ports.loadBMS.send({
      name: file.name,
      header: { bpm: parseFloat(bpm), lnobj: lnobj ? lnobj : null, waves },
      mlens: (b.chart.timeSignatures as any)._values,
      data: b.chart.objects.all(),
    });
  } else {
    app.ports.loadBMS.send({});
  }
});
