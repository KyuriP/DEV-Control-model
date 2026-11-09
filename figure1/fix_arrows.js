// Replace Mermaid's small arrowheads with bigger ones, then save SVG / PDF / PNG.
// usage: node fix_arrows.js <in.svg> <out_basename> <arrow_length_px> <arrow_width_px>
const { chromium } = require(process.env.PW || "playwright");
const fs = require("fs");
const [inSvg, outBase, L = "16", W = "12"] = process.argv.slice(2);

(async () => {
  const browser = await chromium.launch(process.env.CHROME ? { executablePath: process.env.CHROME } : {});
  const page = await browser.newPage({ deviceScaleFactor: 1 });
  const svgText = fs.readFileSync(inSvg, "utf8");
  await page.setContent(`<html><body style="margin:0;background:#fff">${svgText}</body></html>`);
  const fixed = await page.evaluate(([L, W]) => {
    const svg = document.querySelector("svg");
    const NS = "http://www.w3.org/2000/svg";
    for (const p of svg.querySelectorAll("path.flowchart-link")) {
      const hasEnd = (p.getAttribute("marker-end") || "").includes("point") && !/marker-end:\s*none/.test(p.getAttribute("style") || "");
      const hasStart = (p.getAttribute("marker-start") || "").includes("point");
      p.removeAttribute("marker-end"); p.removeAttribute("marker-start");
      const len = p.getTotalLength();
      const back = Math.min(14, len / 2);
      const heads = [];
      if (hasEnd) heads.push([p.getPointAtLength(len - back), p.getPointAtLength(len)]);
      if (hasStart) heads.push([p.getPointAtLength(back), p.getPointAtLength(0)]);
      for (const [from, end] of heads) {
        let dx = end.x - from.x, dy = end.y - from.y; const n = Math.hypot(dx, dy) || 1; dx /= n; dy /= n;
        // Mermaid stops the line a few px before the node
        const tip = { x: end.x + dx * 4, y: end.y + dy * 4 };
        const base = { x: tip.x - dx * L, y: tip.y - dy * L };
        const px = -dy * W / 2, py = dx * W / 2;
        const tri = document.createElementNS(NS, "path");
        tri.setAttribute("d", `M${tip.x},${tip.y} L${base.x + px},${base.y + py} L${base.x - px},${base.y - py} Z`);
        tri.setAttribute("fill", "#6B7780"); tri.setAttribute("stroke", "none");
        tri.setAttribute("transform", p.getAttribute("transform") || "");
        p.parentNode.appendChild(tri);
      }
    }
    return svg.outerHTML;
  }, [Number(L), Number(W)]);
  const out = fixed;
  fs.writeFileSync(outBase + ".svg", out);

  await page.setContent(`<html><body style="margin:0;background:#fff">${out}</body></html>`);
  const box = await page.evaluate(() => { const r = document.querySelector("svg").getBoundingClientRect(); return { w: Math.ceil(r.width), h: Math.ceil(r.height) }; });
  await page.pdf({ path: outBase + ".pdf", width: box.w + "px", height: box.h + "px", printBackground: true, pageRanges: "1" });
  const p2 = await browser.newPage({ deviceScaleFactor: 4, viewport: { width: box.w, height: box.h } });
  await p2.setContent(`<html><body style="margin:0;background:#fff">${out}</body></html>`);
  await p2.locator("svg").screenshot({ path: outBase + ".png" });
  await browser.close();
})();
