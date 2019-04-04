module.exports = {
  excludes: ['**/build/**'],
  environments: ['node', 'browser', 'jest'],
  declarationKeyword: 'import',
  groupImports: false,
  sortImports: false,
  emptyLineBetweenGroups: true,
  importDevDependencies: false,
  danglingCommas: false,
  stripFileExtensions: ['.js', '.jsx', '.ts', '.tsx'],
  useRelativePaths: true,
  maxLineLength: 80,
  importStatementFormatter: function(source) {
    return source.importStatement.replace(/"/g, "'");
  },
  tab: '  ',
  logLevel: 'info',
};
