local M = {}

function M.kill_word()
    local col = vim.api.nvim_win_get_cursor(0)[2]
    local line = vim.api.nvim_get_current_line()

    if #line <= col then
        vim.api.nvim_feedkeys(
            vim.api.nvim_replace_termcodes("<Del><C-o>dw", true, true, true),
            "i",
            true
        )
    else
        vim.api.nvim_feedkeys(
            vim.api.nvim_replace_termcodes("<C-o>dw", true, true, true),
            "i",
            true
        )
    end
end

function M.kill_line()
    local col = vim.api.nvim_win_get_cursor(0)[2]
    local line = vim.api.nvim_get_current_line()

    if #line <= col then
        vim.api.nvim_feedkeys(
            vim.api.nvim_replace_termcodes("<Del>", true, true, true),
            "i",
            true
        )
    else
        vim.api.nvim_feedkeys(
            vim.api.nvim_replace_termcodes("<C-o>d$", true, true, true),
            "i",
            true
        )
    end
end

return M
