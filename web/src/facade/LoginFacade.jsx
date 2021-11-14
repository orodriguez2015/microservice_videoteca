import { LOGIN_API } from '../constantes/Configuracion';

/**
 * 
 * Clase AuthenticateFacade con operaciones relacionadas con peticiones al backend para realizar 
 * autenticación de un usuario u otras
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
export class LoginFacade {

    /**
     * Se envía una petición al servidor para comprobar si un usuario existe 
     * en el sistema. 
     * @param {String} login Login del usuario
     * @param {String} password  Password
     * @return Una promesa
     */
    static authenticate(login,password) {
        let headers =  {
            "Content-Type": "application/json",
            "Access-Control-Request-Headers": "*",
            "Access-Control-Request-Method": "*"
        }

        var opciones = {
            body: JSON.stringify({
                username:login,
                password: password
            }),
            method: 'POST',
            mode: 'cors',
            headers: headers
        }

        return new Promise((resolver, rechazar) => {
            fetch(LOGIN_API,opciones)
            .then((response) => {
                return response.json()
            })
            .then((recurso) => { 
                resolver(recurso);
            }).catch(err=>{ 
                rechazar(err);
            });
        });
    }
}
