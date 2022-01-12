local M = {}

function M.delete_other_buffers()
    local bufs = vim.api.nvim_list_bufs()

    for _, buf in ipairs(bufs) do
        if buf ~= vim.api.nvim_get_current_buf() then
            require("mini.bufremove").delete(buf)
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
            require("mini.bufremove").delete(buf)
            count = count + 1
        end
    end

    vim.api.nvim_echo(
        { { string.format("Killed %d unloaded buffers", count) } },
        true,
        {}
    )
end

function M.new_empty_buffer()
    vim.api.nvim_set_current_buf(vim.api.nvim_create_buf(true, false))
end

return M
