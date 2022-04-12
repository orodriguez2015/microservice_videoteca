/**
 * Action utilizado para autenticar usuario
 * @param {Object} Objeto con los datos del usuario autenticado (id,nombre,apellidos, ...) 
 */
export function autenticarUsuarioAction(user) {
    return {
        type: 'AUTENTICAR_USUARIO',
        usuario: user
    };
}