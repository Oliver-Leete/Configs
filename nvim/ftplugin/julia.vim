let b:doge_parser = 'julia'
let b:doge_insert = 'above'
let b:doge_supported_doc_standards = doge#buffer#get_supported_doc_standards(['juliadocs'])
let b:doge_doc_standard = doge#buffer#get_doc_standard('julia')
let b:doge_patterns = doge#buffer#get_patterns()

let s:function_pattern = {
\  'nodeTypes': ['function_definition'],
\}

call doge#buffer#register_doc_standard('juliadocs', [
\  doge#helpers#deepextend(s:function_pattern, {
\    'parameters': {
\      'format': ':param {name} {type|!type}: !description',
\    },
\    'template': [
\      '"""',
\      '    !functionSigniture',
\      '',
\      '!description',
\      '',
\      '# Arguments',
\      '%(parameters|{parameters})%',
\      '%(returnType|:rtype {returnType}: !description)%',
\      '%(exceptions|{exceptions})%',
\      '',
\      '# Examples',
\      '```jldoctest',
\      'julia> ',
\      '"""',
\    ],
\  }),
\])
