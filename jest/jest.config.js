const { pathsToModuleNameMapper } = require("ts-jest/utils")
const { compilerOptions } = require("../tsconfig")

module.exports = {
  moduleFileExtensions: [
    "ts",
    "tsx",
    "js",
    "jsx",
    "json",
    "node",
    "yaml",
    "yml",
  ],
  transform: {
    "^.+\\.tsx?$": "ts-jest",
    "\\.(jpg|jpeg|png|gif|eot|otf|webp|svg|ttf|woff|woff2|mp4|webm|wav|mp3|m4a|aac|oga)$":
      "<rootDir>/jest/transformers/fileTransformer.js",
    "\\.(yaml|yml)$": "<rootDir>/jest/transformers/yamlTransformer.js",
  },
  testRegex: "(<rootDir>/__tests__/.*|(\\.|/)(test|spec))\\.([tj]sx?)$",
  moduleNameMapper: {
    "\\.(css|less)$": "identity-obj-proxy",
    ...pathsToModuleNameMapper(compilerOptions.paths, { prefix: "<rootDir>/" }),
  },
  testPathIgnorePatterns: ["node_modules", ".cache"],
  transformIgnorePatterns: ["node_modules/(?!(gatsby)/)"],
  testURL: "http://localhost",
  modulePaths: ["<rootDir>"],
  moduleDirectories: [".", "src", "node_modules"],
  rootDir: "../",
  setupFilesAfterEnv: ["<rootDir>/jest/setup-test-env.ts", "jest-extended"],
  roots: ["<rootDir>", "<rootDir>/jest/"],
  globals: {
    "ts-jest": {
      diagnostics: {
        warnOnly: true,
      },
    },
  },
}
