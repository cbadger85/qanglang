// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
import fs from "node:fs";

const langSyntax = fs.readFileSync(
  "../crates/qanglang-ls/syntaxes/qanglang.tmGrammar.json",
  {
    encoding: "utf-8",
  }
);

// https://astro.build/config
export default defineConfig({
  integrations: [
    starlight({
      title: "QangLang Docs",
      social: [
        {
          icon: "github",
          label: "GitHub",
          href: "https://github.com/cbadger85/qanglang/tree/main",
        },
      ],
      sidebar: [
        {
          label: "Getting Started",
          link: "/",
        },
        {
          label: "Concepts",
          items: [
            {
              label: "Types",
              items: [
                { label: "Numbers", slug: "concepts/types/numbers" },
                { label: "Booleans", slug: "concepts/types/booleans" },
                { label: "Strings", slug: "concepts/types/strings" },
                { label: "Nil Values", slug: "concepts/types/nil" },
                { label: "Arrays", slug: "concepts/types/arrays" },
                {
                  label: "Object literals",
                  slug: "concepts/types/object-literals",
                },
              ],
            },
            {
              label: "Control Flow",
              items: [
                {
                  label: "Conditions and Loops",
                  slug: "concepts/control-flow/conditions-and-loops",
                },
                {
                  label: "Returns and Jumps",
                  slug: "concepts/control-flow/returns-and-jumps",
                },
              ],
            },
            {
              label: "Class and Instances",
              slug: "concepts/classes",
            },
            {
              label: "Functions and Lambdas",
              slug: "concepts/functions",
            },
            {
              label: "Nil Safety",
              slug: "concepts/nil-safety",
            },
            {
              label: "Modules and Imports",
              slug: "concepts/modules",
            },
          ],
        },
        {
          label: "API Reference",
          items: [
            {
              label: "Global Functions",
              slug: "reference/api/global-functions",
            },
            {
              label: "Number",
              slug: "reference/api/number-methods",
            },
            {
              label: "String",
              slug: "reference/api/string-methods",
            },
            {
              label: "Array",
              slug: "reference/api/array-methods",
            },
            {
              label: "Function",
              slug: "reference/api/function-methods",
            },
            {
              label: "Iterator",
              slug: "reference/api/number-methods",
            },
            {
              label: "Result",
              slug: "reference/api/number-methods",
            },
            {
              label: "HashMap",
              slug: "reference/api/number-methods",
            },
            {
              label: "HashSet",
              slug: "reference/api/number-methods",
            },
          ],
        },
        {
          label: "Language Reference",
          items: [
            {
              label: "Grammar",
              slug: "reference/language/grammar",
            },
          ],
        },
      ],
      expressiveCode: {
        shiki: {
          langs: [JSON.parse(langSyntax)],
        },
      },
    }),
  ],
});
