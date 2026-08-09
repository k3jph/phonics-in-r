document.addEventListener("DOMContentLoaded", () => {
  document.querySelectorAll(".jh-current-year").forEach((element) => {
    element.textContent = String(new Date().getFullYear());
  });
});
