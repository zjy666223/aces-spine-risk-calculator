(function () {
  "use strict";

  const model = window.COX_MODEL;
  const coefficients = model.coefficients;
  let horizon = 15;

  const ids = [
    "aces_score",
    "age",
    "sex",
    "bmi",
    "education",
    "smoking",
    "household_income",
    "physical_activity",
    "tdi"
  ];

  const continuous = ["aces_score", "age", "bmi", "tdi"];
  const factors = ["sex", "education", "smoking", "household_income", "physical_activity"];

  const labels = {
    aces_score: "ACEs score",
    age: "Age",
    bmi: "BMI",
    tdi: "TDI",
    sex: "Sex",
    education: "Education",
    smoking: "Smoking",
    household_income: "Household income",
    physical_activity: "Physical activity"
  };

  const units = {
    aces_score: "points",
    age: "years",
    bmi: "kg/m2",
    tdi: "units"
  };

  const $ = (id) => document.getElementById(id);

  function clamp(value, min, max) {
    return Math.min(Math.max(value, min), max);
  }

  function coefficientForFactor(name, value) {
    const key = `${name}${value}`;
    return Number(coefficients[key] || 0);
  }

  function profileFromInputs() {
    return {
      aces_score: Number($("aces_score").value),
      age: Number($("age").value),
      sex: $("sex").value,
      bmi: Number($("bmi").value),
      education: $("education").value,
      smoking: $("smoking").value,
      household_income: $("household_income").value,
      physical_activity: $("physical_activity").value,
      tdi: Number($("tdi").value)
    };
  }

  function linearPredictor(profile) {
    let value = 0;
    for (const key of continuous) {
      value += Number(coefficients[key] || 0) * Number(profile[key] || 0);
    }
    for (const key of factors) {
      value += coefficientForFactor(key, profile[key]);
    }
    return value;
  }

  function riskFor(profile, years) {
    const h0 = Number(model.baselineCumHaz[String(years)] || 0);
    const eta = linearPredictor(profile);
    const cumulativeHazard = h0 * Math.exp(eta);
    return clamp(1 - Math.exp(-cumulativeHazard), 0, 0.999);
  }

  function formatPct(value) {
    const pct = value * 100;
    if (pct < 1) return `${pct.toFixed(2)}%`;
    if (pct < 10) return `${pct.toFixed(1)}%`;
    return `${pct.toFixed(0)}%`;
  }

  function formatSigned(value) {
    const sign = value >= 0 ? "+" : "";
    return `${sign}${value.toFixed(2)}`;
  }

  function riskCategory(risk) {
    const q = model.riskQuantiles[String(horizon)];
    if (risk < Number(q.q33)) return { label: "Low", className: "low" };
    if (risk < Number(q.q67)) return { label: "Moderate", className: "moderate" };
    if (risk < Number(q.q90)) return { label: "High", className: "high" };
    return { label: "Very high", className: "very-high" };
  }

  function factorContribution(key, profile, reference) {
    return coefficientForFactor(key, profile[key]) - coefficientForFactor(key, reference[key]);
  }

  function contributions(profile) {
    const reference = model.referenceProfile;
    const rows = [];

    for (const key of continuous) {
      const beta = Number(coefficients[key] || 0);
      const value = Number(profile[key] || 0);
      const ref = Number(reference[key] || 0);
      rows.push({
        key,
        label: labels[key],
        valueLabel: `${value}${key === "bmi" ? "" : key === "tdi" ? "" : " " + units[key]}`,
        contribution: beta * (value - ref)
      });
    }

    for (const key of factors) {
      rows.push({
        key,
        label: `${labels[key]}: ${profile[key]}`,
        valueLabel: `ref ${reference[key]}`,
        contribution: factorContribution(key, profile, reference)
      });
    }

    return rows
      .filter((row) => Math.abs(row.contribution) > 0.004)
      .sort((a, b) => Math.abs(b.contribution) - Math.abs(a.contribution))
      .slice(0, 7);
  }

  function renderContributors(profile) {
    const list = $("contributors-list");
    list.textContent = "";
    const rows = contributions(profile);

    if (rows.length === 0) {
      const empty = document.createElement("p");
      empty.className = "empty-state";
      empty.textContent = "This profile matches the reference profile closely.";
      list.appendChild(empty);
      return;
    }

    const maxAbs = Math.max(...rows.map((row) => Math.abs(row.contribution)), 0.01);
    for (const row of rows) {
      const item = document.createElement("div");
      item.className = "contributor";

      const top = document.createElement("div");
      top.className = "contributor-top";

      const name = document.createElement("span");
      name.textContent = row.label;

      const value = document.createElement("span");
      value.className = "contributor-value";
      value.textContent = `${formatSigned(row.contribution)} log-HR`;

      const track = document.createElement("div");
      track.className = "bar-track";

      const fill = document.createElement("span");
      fill.className = `bar-fill${row.contribution < 0 ? " negative" : ""}`;
      fill.style.width = `${Math.max(8, (Math.abs(row.contribution) / maxAbs) * 100)}%`;

      top.appendChild(name);
      top.appendChild(value);
      track.appendChild(fill);
      item.appendChild(top);
      item.appendChild(track);
      list.appendChild(item);
    }
  }

  function render() {
    const profile = profileFromInputs();
    const reference = model.referenceProfile;
    const risk = riskFor(profile, horizon);
    const refRisk = riskFor(reference, horizon);
    const relativeHr = Math.exp(linearPredictor(profile) - linearPredictor(reference));
    const category = riskCategory(risk);

    $("aces_score_value").textContent = profile.aces_score;
    $("tdi_value").textContent = profile.tdi.toFixed(1);
    $("risk-subtitle").textContent = `${horizon}-year incident spine degenerative disease risk`;
    $("risk-percent").textContent = formatPct(risk);
    $("reference-risk").textContent = formatPct(refRisk);
    $("relative-hr").textContent = relativeHr.toFixed(2);
    $("risk-category").textContent = category.label;
    $("risk-category").className = `risk-badge ${category.className}`;

    const q = model.riskQuantiles[String(horizon)];
    const maxRisk = Math.max(Number(q.q95) * 1.35, risk * 1.12, 0.025);
    const left = clamp((risk / maxRisk) * 100, 0, 100);
    $("risk-marker").style.left = `${left}%`;

    document.querySelectorAll(".horizon-toggle button").forEach((button) => {
      button.classList.toggle("active", Number(button.dataset.horizon) === horizon);
    });

    renderContributors(profile);
  }

  function setProfile(profile) {
    for (const key of ids) {
      if ($(key)) $(key).value = profile[key];
    }
    render();
  }

  function applyRanges() {
    for (const [key, range] of Object.entries(model.inputRanges)) {
      const el = $(key);
      if (!el) continue;
      el.min = range.min;
      el.max = range.max;
      el.step = range.step;
    }
  }

  function populateMetrics() {
    $("metric-cindex").textContent = Number(model.metrics.coxTestCIndex).toFixed(3);
    $("metric-auc").textContent = Number(model.metrics.bestMlAuc).toFixed(3);
    $("xgb-auc").textContent = Number(model.metrics.xgboostAuc).toFixed(3);
  }

  function init() {
    applyRanges();
    populateMetrics();
    setProfile(model.referenceProfile);

    ids.forEach((key) => {
      const el = $(key);
      if (!el) return;
      el.addEventListener("input", render);
      el.addEventListener("change", render);
    });

    document.querySelectorAll(".horizon-toggle button").forEach((button) => {
      button.addEventListener("click", () => {
        horizon = Number(button.dataset.horizon);
        render();
      });
    });

    $("reset-profile").addEventListener("click", () => setProfile(model.referenceProfile));
    $("example-profile").addEventListener("click", () => setProfile({
      aces_score: 4,
      age: 64,
      sex: "Female",
      bmi: 33.5,
      education: "Other",
      smoking: "Former",
      household_income: "Low",
      physical_activity: "Low",
      tdi: 3.2
    }));
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
