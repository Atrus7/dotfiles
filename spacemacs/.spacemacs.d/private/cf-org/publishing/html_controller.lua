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

-- For DOCX: apply text-align=center to the first paragraph
local function center_div_docx(el)
  local first = el.content[1]
  if first and (first.t == "Para" or first.t == "Plain") then
    el.content[1] = pandoc.Para(
      first.content,
      pandoc.Attr("", {}, {["text-align"] = "center"})
    )
  end
  return el
end
