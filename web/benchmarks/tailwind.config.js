module.exports = {
  content: [
    "./src/**/*.{html,js}",
    "../../utils/benchmark-report.py"
  ],
  theme: {
    extend: {
      colors: {
        ink: "#0f172a",
        steel: "#334155",
        mist: "#e2e8f0",
        paper: "#f8fafc",
        line: "#cbd5e1",
        ok: "#166534",
        okBg: "#dcfce7",
        warn: "#92400e",
        warnBg: "#fef3c7",
        bad: "#991b1b",
        badBg: "#fee2e2",
        accent: "#0f766e",
        accentBg: "#ccfbf1"
      },
      boxShadow: {
        panel: "0 18px 50px rgba(15, 23, 42, 0.08)"
      }
    }
  },
  plugins: []
};
