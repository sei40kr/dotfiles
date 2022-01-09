local M = {}

local function check_backspace()
    local col = vim.api.nvim_win_get_cursor(0)[2]
    local line = vim.api.nvim_get_current_line()

    return col == 0 or line:sub(col, col):find("%s") ~= nil
end

function M.tab_complete()
    local cmp = require("cmp")
    local luasnip = require("luasnip")

    if luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
    elseif cmp.visible() then
        cmp.select_next_item()
    elseif check_backspace() then
        vim.api.nvim_feedkeys(
            vim.api.nvim_replace_termcodes("<Tab>", true, true, true),
            "n",
            true
        )
    end

    cmp.complete()
end

function M.s_tab_complete()
    local cmp = require("cmp")
    local luasnip = require("luasnip")

    if luasnip.jumpable(-1) then
        luasnip.jump(-1)
    elseif cmp.visible() then
        cmp.select_prev_item()
    end

    vim.api.nvim_feedkeys(
        vim.api.nvim_replace_termcodes("<S-Tab>", true, true, true),
        "n",
        true
    )
end

function M.complete_buffer()
    require("cmp").complete({ config = { sources = { { name = "buffer" } } } })
end

function M.complete_omni()
    require("cmp").complete({ config = { sources = { { name = "omni" } } } })
end

function M.complete_path()
    require("cmp").complete({ config = { sources = { { name = "path" } } } })
end

function M.complete_spell()
    require("cmp").complete({ config = { sources = { { name = "spell" } } } })
end

return M
