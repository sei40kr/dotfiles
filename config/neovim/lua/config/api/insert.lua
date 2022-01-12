local M = {}

function M.insert_newline_above()
    local lines = {}

    for _ = 1, vim.v.count1 do
        table.insert(lines, "")
    end

    vim.api.nvim_put(lines, "l", false, true)
end

function M.insert_newline_below()
    local pos = vim.api.nvim_win_get_cursor(0)
    local lines = {}

    for _ = 1, vim.v.count1 do
        table.insert(lines, "")
    end

    vim.api.nvim_put(lines, "l", true, false)

    vim.api.nvim_win_set_cursor(0, pos)
end

return M
