#!/usr/bin/env nix-shell
#! nix-shell -i nu --pure
#! nix-shell --packages nushell incus
#! nix-shell -I nixpkgs=channel:nixos-25.11-small
use std/log

const images_url = 'https://images.linuxcontainers.org'
const images_protocol = 'simplestreams'
const remote_name = 'local'
const remote = $'($remote_name):'
const instance_name = 'nixos'
const instance = $'($remote_name):($instance_name)'
const nixos_version = '25.11'

const incus_status = {
    102: stopped,
    103: running,
    stopped: 102,
    running: 103,
}

const needed_profiles = {
    cpu-1: {
        description: 'ensure only 1 cpu is used',
        config: {
            'limits.cpu': '1-1',
        },
    },
    mem-1gib: {
        description: 'prevent instance from using more than 1 GiB of memory',
        config: {
            'limits.memory': '1GiB',
            'limits.memory.enforce': hard,
            'limits.memory.swap': '0B',
        },
    },
    nesting: {
        config: {
            'security.nesting': 'true',
        },
        description: 'support running incus nested in an instance',
    },
    no-auto-start: {
        config: {
            'boot.autostart': 'false',
        },
        description: 'prevent from starting on bootup',
    },
}


# use incus to download and do basic setup for a nixos instance
def main [
    --delete-images, # remove all nixos images that have already been downloaded, and download a new one
    --delete-instance, # remove already-configured instance, and recreate it
    --refresh-images, # if nixos images are already present, update them
]: [nothing -> nothing] {
    log debug $'we will (if $delete_images {""} else {"not "})remove every NixOS image'
    log debug $'we will (if $delete_instance {""} else {"not "})remove every instance names nixos'
    log debug $'we will (if $refresh_images {""} else {"not "})refresh all nixos images'

    let instances = {||
        incus list $remote --format json |
        from json
    }
    if ((do $instances) | where name == 'nixos' | is-not-empty) and $delete_instance {
        log info $'deleting instance: ($instance)'
        incus delete --force $instance
    }
    let images = {||
        incus image list $remote --format json |
        from json
    }
    let nixos_images = {|| do $images | where properties.os like '(?i)^nixos$'}
    if (do $nixos_images | is-not-empty) and $delete_images {
        log warning 'removing nixos images'
        incus image delete ...(
            $nixos_images |
            each {|it|
                $'($remote_name):($it.fingerprint)'
            }
        )
    }
    if (do $nixos_images | is-not-empty) and $refresh_images {
        log info 'refreshing all nixos images'
        incus image refresh ...(
            $nixos_images |
            each {|it|
                $'($remote_name):($it.fingerprint)'
            }
        )
    } else if (do $nixos_images | is-empty) {
        log info 'downloading new nixos image'
        incus image copy $'images:nixos/($nixos_version)' $remote --copy-aliases
    }

    create-or-update profiles

    if ((do $instances) | where name == 'nixos' | is-not-empty) {
        log info $'instance ($instance) exists, so removing and re-adding profiles'
        let current_attached = (
            incus list $instance --format json |
            from json |
            first |
            get profiles
        )
        log debug $'these profiles are currently attached: ($current_attached | to nuon)'
        let extra = ($current_attached | where {$in not-in $needed_profiles})
        let missing = ($needed_profiles | columns | where {$in not-in $current_attached})
        if ($extra | is-not-empty) {
            $extra |
            each {|name|
                log info $'removing extra profile from ($instance) -> ($name)'
                incus profile remove $instance $name
            }
        }
        if ($missing | is-not-empty) {
            $missing |
            each {|name|
                log info $'adding missing profile -> ($name)'
                incus profile add $instance $name
            }
        }
    } else {
        log info $'creating instance ($instance)'
        let most_recent_image = (
            do $nixos_images |
            sort-by --reverse {get created_at | into datetime} |
            first
        )
        run-external 'incus' ...([
            create
            $'($remote_name):($most_recent_image.fingerprint)'
            $instance
        ] | append (
            $needed_profiles |
            append 'default' |
            columns |
            each {|it| [--profile $'($remote_name):($it)']} |
            flatten
        ))
    }
}

# create or update the profiles needed to run the nixos instance
def "create-or-update profiles" []: [nothing -> nothing] {
    let expected_profiles = (
        $needed_profiles |
        transpose name value |
        each {|it| $it.value | merge {name: $it.name}}
    )
    let needs_updating = {|it|
        let profile = $it
        let expected = (
            $expected_profiles |
            where {|ij| $ij.name == $profile.name} |
            first
        )
        ($profile | to nuon) != ($profile | merge $expected | to nuon)
    }
    log info 'checking if any profiles are being used by any instances other than ones named "nixos", and would need to be updated...'
    let profiles = (
        incus profile list $remote --format json |
        from json |
        where name in $needed_profiles |
        where {$in.used_by | any {$in not-like $'($instance_name)$'}}
    )
    if ($profiles | any $needs_updating) {
        return (error make {
            msg: $"the following profiles need to be updated, but are being used by other instances:\n($profiles | where $needs_updating | each {to yaml} | str join "\n")",
        })
    }
    log info "stopping instance to update profiles, in case it's using them"
    stop instance $instance
    log debug 'getting list of profile names'
    let current_profile_names = (
        incus profile list $remote --format json |
        from json |
        get name
    )
    $expected_profiles | each {|it|
        if ($it.name in $current_profile_names) {
            log info $'updating profile: ($it.name)'
            $it | to yaml | incus profile edit $'($remote_name):($it.name)'
        } else {
            log info $'creating profile: ($it.name)'
            $it | to yaml | incus profile create $'($remote_name):($it.name)'
        }
    }
}

def "stop instance" [inst: string]: [nothing -> nothing] {
    if (incus list $inst --format json | from json | is-empty) {
        log info $'instance does not exist, no need to stop: ($inst)'
        return null
    }
    let instance_status = {||
        incus list $inst --format json |
        from json |
        first |
        get state.status_code |
        into int
    }
    if ((do $instance_status) == $incus_status.stopped) {
        return null
    }
    log info $'waiting 20sec for ($inst) to stop'
    incus stop $inst --timeout 20
    if ((do $instance_status) != $incus_status.stopped) {
        return (error make {
            msg: $'could not stop ($inst) in 20sec',
        })
    }
}

def "setup images remote" [] {
    log info 'checking on incus remotes...'
    let remotes = (
        incus remote list --format json |
        from json
    )
    log info ("found\n" + ($remotes | table -e))
    if not ($remotes has 'images') {
        log info 'adding remotes "images"'
        incus remote add images $images_url --protocol $images_protocol --public
    }
    if not ($remotes.images.Protocol == $images_protocol) {
        log warning $'"images" protocol: ($remotes.images.Protocol); expected ($images_protocol)'
        log warning 'moving current "images" to "images_old"'
        if ($remotes has 'images_old') {
            return (error make {
                msg: '"images_old" already exists!',
            })
        }
        incus remote rename images images_old
        incus remote add images $images_url --protocol $images_protocol --public
    }
    if not ($remotes.images.Addr? == $images_url) {
        log warning $'changing "images" url: ($remotes.images.Addr) -> ($images_url)'
        incus remote set-url images $images_url
    }
    log info 'using "images"'
    incus remote list --format json | from json | select images | table -e
}
