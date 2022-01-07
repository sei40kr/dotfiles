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

return M
