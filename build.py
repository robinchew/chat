import json
import os

from eluthia.decorators import chmod, copy_files, copy_folder, file, git_clone, empty_folder, build_and_unpack_erlang_release
from eluthia.defaults import control
from eluthia.functional import pipe
from eluthia.py_configs import deb822, nginx

DOMAIN = 'chat.robin.au'
DOMAIN2 = 'chat2.robin.au'

INTERNAL_PORT = 9000
PUBLIC_PORT = 80

DEFAULT_ENV = {
    'WS_PROTOCOL': 'ws',
    'WS_HOST': DOMAIN2,
    'WS_PORT': PUBLIC_PORT,
}

@chmod(0o755)
@file
def postinst(full_path, package_name, apps):
    return f'''\
        #!/bin/bash
        systemctl daemon-reload

        # Enable and start the service
        systemctl enable {package_name}
        systemctl restart {package_name} nginx
    '''

@file
def systemd_service(full_path, package_name, apps):
    environment_variables = '\n'.join(
        f"        Environment={variable}={value}"
        for variable, value in {**DEFAULT_ENV, **apps[package_name]['env']}.items()).strip()

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
    env_json = json.dumps(DEFAULT_ENV)
    env_init_location = ('location', '=', '/importEnv.js', (
        ('return', 200, f"'importEnv = () => ({env_json})'"),
    ))
    return (
        ('server', (
            ('listen', PUBLIC_PORT),
            ('server_name', DOMAIN, DOMAIN2),
            ('charset', 'utf-8'),

            # root must be set at server level in order for "location = /" to work
            ('root', '/home/ubuntu/system/chat_front'),

            ('location', '=', '/', (
                ('try_files', '$uri', '/index.html'),
            )),
            env_init_location,
            ('location', '/', (
                ('proxy_pass', f"http://localhost:{INTERNAL_PORT}"),
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
