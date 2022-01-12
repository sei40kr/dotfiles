local M = {}

function M.find_file()
    require("telescope").extensions.file_browser.file_browser({
        cwd = vim.fn.expand("%:p:h"),
    })
end

function M.find_file_from_here()
    require("telescope.builtin").find_files({ cwd = vim.fn.expand("%:p:h") })
end

return M
