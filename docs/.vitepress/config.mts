import { defineConfig } from "vitepress";

// https://vitepress.dev/reference/site-config
export default defineConfig({
  title: "Kola",
  description: "Kola Documentation",
  srcDir: "./src",
  base: "/kola/",
  themeConfig: {
    // https://vitepress.dev/reference/default-theme-config
    nav: [
      { text: "Guide", link: "/guide/about" },
      { text: "Reference", link: "/reference/cli" },
    ],

    sidebar: {
      "/guide/": {
        base: "/guide/",
        items: [
          {
            text: "Introduction",
            collapsed: false,
            items: [
              { text: "What is Kola?", link: "about" },
              { text: "Getting Started", link: "quickstart" },
            ],
          },
        ],
      },
      "reference/": {
        base: "/reference/",
        items: [
          {
            text: "Reference",
            items: [{ text: "CLI", link: "cli" }],
          },
        ],
      },
    },

    socialLinks: [{ icon: "github", link: "https://github.com/mordragt/kola" }],

    footer: {
      message: "Released under the MIT License.",
      copyright: "Copyright © 2025-present Thomas Wehmöller",
    },
  },
});
