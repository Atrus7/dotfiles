io.stderr:write(">> html_controller.lua loaded for format: ", FORMAT, "\n")

-- dialogue.lua  –  convert definition-list “dialogue” into one-line paragraphs
--   Org source:  - speaker :: Speech
--   Output:      <p><strong>speaker</strong> — Speech</p>

local dash = pandoc.Str '—'

-- build the <strong>speaker</strong> — speech … paragraph
local function make_para(label_inlines, speech_inlines)
  local inl = pandoc.List:new()            -- empty inline list
  inl:extend{ pandoc.Strong(label_inlines), pandoc.Space(), dash, pandoc.Space() }
  inl:extend(speech_inlines)               -- append the actual speech
  return pandoc.Para(inl)
end

function DefinitionList(dl)
  local out = pandoc.Blocks{}
  for _, item in ipairs(dl.content) do
    local label_inlines = item[1]          -- list<Inline>
    local defs          = item[2]          -- list< list<Block> >
    local first_block   = defs[1] and defs[1][1]      -- may be nil

    -- default: empty speech until we find a Para or Plain block
    local speech_inlines = pandoc.Inlines{}
    if first_block and (first_block.t == 'Para' or first_block.t == 'Plain') then
      speech_inlines = first_block.content
    end

    out:insert(make_para(label_inlines, speech_inlines))
  end
  return out                               -- replace the whole DefinitionList
end


-- For HTML: apply style="text-align:center;" to the <div>
local function center_div_html(el)
  -- 1. copy existing Attr pieces ------------------------------------
  local id        = el.attr.identifier      -- string  (may be "")
  local classes   = el.attr.classes         -- list    (array-like)
  local kvs_old   = el.attr.attributes      -- table   (key → value)
  local kvs = {}                            -- we’ll build a fresh table

  for k, v in pairs(kvs_old) do kvs[k] = v end  -- shallow copy

  -- 2. append / extend a style attribute ----------------------------
  if kvs["style"] then
    kvs["style"] = kvs["style"] .. " text-align:center;"
  else
    kvs["style"] = "text-align:center;"
  end

  -- 3. assign new Attr object back onto the Div ---------------------
  el.attr = pandoc.Attr(id, classes, kvs)

  return el      -- keep wrapper; children untouched
end

function print_table(tbl, indent)
  indent = indent or ""
  for k, v in pairs(tbl) do
    local key = tostring(k)
    local val = type(v) == "table" and "<table>" or tostring(v)
    io.stderr:write(indent .. key .. " = " .. val .. "\n")
    if type(v) == "table" then
      print_table(v, indent .. "  ")
    end
  end

end
-- helper: copy a Lua table
local function tcopy(src)
   local dst = {}
   for k,v in pairs(src) do dst[k] = v end
   return dst
end


-- For DOCX: apply text-align=center to the first paragraph
local function center_div_docx(el)
  local first = el.content[1]

  if first and (first.t == "Para" or first.t == "Plain") then
     -- convert any Code inlines to Str so Word drops the VerbatimChar style
     local inl = pandoc.List:new()
     for _,x in ipairs(first.content) do
        inl:insert(x.t == "Code" and pandoc.Str(x.text) or x)
     end


     print_table(el)

     io.stderr:write("----\n")
     print_table(el.content[1])
     -- print_table(el.content[1].attr)


     local p = pandoc.Para(first.content, pandoc.Attr("", {}, {["text-align"] = "center"}))

     -- p.attr = nil
     -- p.attr = pandoc.Attr("", {}, {["text-align"] = "center"})

     -- add the DOCX-relevant alignment attribute
     -- p.attr = { "", {}, {["text-align"] = "center"} }

     -- el.attr.attributes = {["text-align"] = "center"}
     -- el.attr = p.attr
     io.stderr:write(tostring(el.attr))
     -- io.stderr:write(tostring(p.attr))


     -- p.attr = {
     --    identifier = "",
     --    classes    = {},
     --    attributes = {["text-align"] = "center"}
     -- }
     -- print_table(p)

     print_table(p)
     el.content[1] = p
     -- el.content[1].attr.attributes="test"
     io.stderr:write("has attr? ", tostring(el.content[1].attr ~= nil), "\n")
     if el.content[1].attr then
        local id, cls, kv = el.content[1].attr.identifier,
           el.content[1].attr.classes,
           el.content[1].attr.attributes
        io.stderr:write("  text-align = ", kv["text-align"] or "nil", "\n")
     end


     -- print_table(el.attr)
     print_table(el.content[1])
     io.stderr:write("----\n")
     -- print_table(el.content[1].attr)
  end
  return el
end

-- center.lua – replaces Div(class=center) with <p style="text-align:center">...</p>
function Div(el)

  if not el.classes:includes("center") then return nil end   -- nothing to do
  if FORMAT == "html" or FORMAT:match("html") then
    return center_div_html(el)
  elseif FORMAT == "docx" then
    return center_div_docx(el)
  end

  return el  -- pass through for other formats
end
