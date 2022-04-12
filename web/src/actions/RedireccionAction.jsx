/**
 * Action utilizado para realizar una redirección
 * @param {Object} String con la ruta a la que se pretende realizar la redirección
 */
export function redireccionAction(destino) {
    return {
        type: 'REDIRECCION_ACTION',
        destino: destino
    };
}