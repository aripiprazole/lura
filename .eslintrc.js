
module.exports = {
  env: {
    browser: false,
    es2021: true,
    mocha: true,
    node: true,
  },
  extends: ['google'],
  parser: '@typescript-eslint/parser',
  parserOptions: {
    ecmaFeatures: {
      jsx: true,
    },
    ecmaVersion: 12,
    sourceType: 'module',
  },
  plugins: ['prettier', '@typescript-eslint'],
  settings: {
    react: {
      version: 'detect',
    },
  },
  rules: {
    'require-jsdoc': ['off'],
    'react/prop-types': ['off'],
    'spaced-comment': ['off'],
    'prettier/prettier': ['error', {}, {usePrettierrc: true}],
    'react/no-children-prop': ['off'],
    'indent': ['off'],
    'quotes': ['off'],
    'max-len': ['off'],
    'no-unused-vars': ['off'],
    'operator-linebreak': ['error', 'after', {overrides: {'?': 'before', ':': 'before'}}],
  },
};