local M = {}

function M.delete_other_buffers()
    local bufs = vim.api.nvim_list_bufs()

    for _, buf in ipairs(bufs) do
        if buf ~= vim.api.nvim_get_current_buf() then
            vim.api.nvim_buf_delete(buf, {})
        end
    end

    vim.api.nvim_echo(
        { { string.format("Killed %d other buffers", #bufs - 1) } },
        true,
        {}
    )
end

function M.delete_unloaded_buffers()
    local bufs = vim.api.nvim_list_bufs()
    local count = 0

    for _, buf in ipairs(bufs) do
        if not vim.api.nvim_buf_is_loaded(buf) then
            vim.api.nvim_buf_delete(buf, {})
            count = count + 1
        end
    end

    vim.api.nvim_echo(
        { { string.format("Killed %d unloaded buffers", count) } },
        true,
        {}
    )
end

return M
