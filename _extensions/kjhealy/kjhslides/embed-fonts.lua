local function readMeta(meta)
    if meta['embed-fonts'] then
        quarto.doc.add_html_dependency({
            name = "fonts",
            version = "0.0",
            stylesheets = { "fonts-embed.css" }
        })
        quarto.doc.add_format_resource("fonts")
    else
        quarto.doc.add_html_dependency({
            name = "fonts-download",
            version = "0.0",
            stylesheets = { "fonts-download.css" }
        })
    end
end

return {
    { Meta = readMeta }
}
