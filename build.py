import json
import os

from eluthia.decorators import chmod, copy_files, copy_folder, file, git_clone, empty_folder, build_and_unpack_erlang_release
from eluthia.defaults import control
from eluthia.functional import pipe
from eluthia.py_configs import deb822, nginx


@chmod(0o755)
@file
def postinst(full_path, package_name, apps):
    return f'''\
        #!/bin/bash
        systemctl daemon-reload

        # Enable and start the service
        systemctl enable {package_name}
        systemctl restart {package_name}
    '''

@file
def systemd_service(full_path, package_name, apps):
    default_env = {}
    environment_variables = '\n'.join(
        f"        Environment={variable}={value}"
        for variable, value in {**default_env, **apps[package_name]['env']}.items()).strip()

    return f'''\
        [Unit]
        Description=Chat Service
        After=network.target
        [Service]
        Type=simple
        User=ubuntu
        ExecStart=/home/ubuntu/system/chat_server/bin/erlchat_release foreground
        {environment_variables}
        [Install]
        WantedBy=multi-user.target
    '''

def nginx_conf(full_path, package_name, apps):
    env = apps[package_name]['env']
    env_json = json.dumps(env)
    env_init_location = ('location', '=', '/importEnv.js', (
        ('return', 200, f'"importEnv = () => ({env_json})"'),
    ))
    return (
        ('server', (
            ('listen', 80),
            ('server_name', 'chat.robin.au'),
            ('charset', 'utf-8'),
            ('location', '=', '/', (
                ('root', '/home/ubuntu/system/chat_front'),
                ('try_files', '$uri', '/index.html'),
            )),
            env_init_location,
            ('location', '/', (
                ('proxy_pass', f"http://localhost:{env['WS_PORT']}"),
            )),
        )),
    )

def get_package_tree(package_name, apps):
    return {
        'DEBIAN': {
            'postinst': postinst,
            'control': file(pipe(
                control,
                lambda d: {**d, 'Description': 'Chat Programme'},
                deb822)),
        },
        'home': {
            'ubuntu': {
                'system': {
                    'chat_front': copy_files(os.path.join(apps[package_name]['folder'], 'frontend'), [
                        'index.html',
                    ]),
                    'chat_server': build_and_unpack_erlang_release(os.path.join(apps[package_name]['_app_folder'], 'erlchat')),
                }
            },
        },
        'etc': {
            'systemd': {
                'system': {
                    f'{package_name}.service': systemd_service,
                },
            },
            'nginx': {
                'sites-enabled': {
                    package_name: file(pipe(
                        nginx_conf,
                        nginx)),
                },
            },
        },
    }
