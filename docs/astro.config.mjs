// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
import fs from "node:fs";
import starlightThemeNext from "starlight-theme-next";

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
      plugins: [starlightThemeNext()],
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
              label: "Entry",
              slug: "reference/api/entry",
            },
            {
              label: "Globals",
              slug: "reference/api/global-functions",
            },
            {
              label: "HashMap",
              slug: "reference/api/hashmap",
            },
            {
              label: "HashSet",
              slug: "reference/api/hashset",
            },
            {
              label: "Intrinsics",
              items: [
                {
                  label: "Array",
                  slug: "reference/api/intrinsics/array-methods",
                },
                {
                  label: "Function",
                  slug: "reference/api/intrinsics/function-methods",
                },
                {
                  label: "Number",
                  slug: "reference/api/intrinsics/number-methods",
                },
                {
                  label: "String",
                  slug: "reference/api/intrinsics/string-methods",
                },
              ],
            },
            {
              label: "Iterators",
              items: [
                {
                  label: "Chain Iterator",
                  slug: "reference/api/iterators/chain-iterator",
                },
                {
                  label: "Enumerate Iterator",
                  slug: "reference/api/iterators/enumerate-iterator",
                },
                {
                  label: "Filter Indexed Iterator",
                  slug: "reference/api/iterators/filter-indexed-iterator",
                },
                {
                  label: "Filter Iterator",
                  slug: "reference/api/iterators/filter-iterator",
                },
                {
                  label: "FlatMap Iterator",
                  slug: "reference/api/iterators/flatmap-iterator",
                },
                {
                  label: "Iterator",
                  slug: "reference/api/iterators/iterator",
                },
                {
                  label: "Map Indexed Iterator",
                  slug: "reference/api/iterators/map-indexed-iterator",
                },
                {
                  label: "Map Iterator",
                  slug: "reference/api/iterators/map-iterator",
                },
                {
                  label: "Take Iterator",
                  slug: "reference/api/iterators/take-iterator",
                },
                {
                  label: "Zip Iterator",
                  slug: "reference/api/iterators/zip-iterator",
                },
              ],
            },
            {
              label: "Pair",
              slug: "reference/api/pair",
            },
            {
              label: "Result",
              slug: "reference/api/result",
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
