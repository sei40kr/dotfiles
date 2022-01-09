vim.api.nvim_set_keymap(
    "n",
    "gs+",
    '<Cmd>lua require("hop").hint_lines_skip_whitespace({ direction = require("hop.hint").HintDirection.AFTER_CURSOR })<CR>',
    { noremap = true }
)
vim.api.nvim_set_keymap(
    "n",
    "gs-",
    '<Cmd>lua require("hop").hint_lines_skip_whitespace({ direction = require("hop.hint").HintDirection.BEFORE_CURSOR })<CR>',
    { noremap = true }
)
vim.api.nvim_set_keymap(
    "n",
    "gsb",
    '<Cmd>lua require("hop").hint_words({ direction = require("hop.hint").HintDirection.BEFORE_CURSOR })<CR>',
    { noremap = true }
)
vim.api.nvim_set_keymap(
    "n",
    "gse",
    '<Cmd>lua require("hop").hint_words({ direction = require("hop.hint").HintDirection.AFTER_CURSOR })<CR>',
    { noremap = true }
)
vim.api.nvim_set_keymap(
    "n",
    "gsf",
    '<Cmd>lua require("hop").hint_char1({ direction = require("hop.hint").HintDirection.AFTER_CURSOR })<CR>',
    { noremap = true }
)
vim.api.nvim_set_keymap(
    "n",
    "gsF",
    '<Cmd>lua require("hop").hint_char1({ direction = require("hop.hint").HintDirection.BEFORE_CURSOR })<CR>',
    { noremap = true }
)
vim.api.nvim_set_keymap(
    "n",
    "gsj",
    '<Cmd>lua require("hop").hint_lines({ direction = require("hop.hint").HintDirection.AFTER_CURSOR })<CR>',
    { noremap = true }
)
vim.api.nvim_set_keymap(
    "n",
    "gsk",
    '<Cmd>lua require("hop").hint_lines({ direction = require("hop.hint").HintDirection.BEFORE_CURSOR })<CR>',
    { noremap = true }
)
vim.api.nvim_set_keymap(
    "n",
    "gss",
    '<Cmd>lua require("hop").hint_char2()<CR>',
    { noremap = true }
)
