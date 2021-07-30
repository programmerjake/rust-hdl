#!/bin/bash
function extract_debug_tokens()
{
    local lines
    mapfile -t lines
    echo '#![no_implicit_prelude]'
    local output=0
    local next_active=1
    local line
    local block_kind=''
    for line in "${lines[@]}"; do
        local next_block_kind="$block_kind"
        if [[ "$line" =~ ^'#[rust_hdl(crate' ]]; then
            next_active=0
            output=0
        elif [[ "$line" =~ ^'--------INPUT: Value'$ ]]; then
            ((output = next_active))
            next_block_kind='IN'
        elif [[ "$line" =~ ^'--------INPUT: FixedTypeValue'$ ]]; then
            next_block_kind='IN'
        elif [[ "$line" =~ ^'--------OUTPUT: Value'$ ]]; then
            ((output = next_active))
            next_active=1
            next_block_kind='OUT'
        elif [[ "$line" =~ ^'--------OUTPUT: FixedTypeValue'$ ]]; then
            ((output = next_active))
            next_active=1
            next_block_kind='OUT'
        elif [[ "$line" =~ ^'--------'$ ]]; then
            output=0
            block_kind=''
            next_block_kind=''
        elif ((output)); then
            printf "%s\n" "$line"
        fi
        if [[ "$block_kind" != "" ]]; then
            echo -n "$block_kind" >&2
            if ((output == 0)); then
                echo -n " (ignored)" >&2
            fi
            echo -n ": " >&2
        fi
        block_kind="$next_block_kind"
        printf "%s\n" "$line" >&2
    done
}
cargo build --features=rust-hdl-macros/debug-tokens "$@" |& extract_debug_tokens