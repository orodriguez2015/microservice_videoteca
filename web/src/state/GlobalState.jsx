import { combineReducers } from 'redux';
import autenticarUsuarioReducer from '../reducers/AutenticarUsuarioReducer';

/**
 * Objeto que combina los reducers para mantener el estado global de la aplicación
 */
export const GlobalState = combineReducers({
    usuario: autenticarUsuarioReducer
});