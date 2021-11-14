import { createStore,compose,applyMiddleware } from 'redux';
import { createBrowserHistory } from 'history';
import thunk from 'redux-thunk';
import { connectRouter,routerMiddleware } from 'connected-react-router';
import {ESTADO_USUARIO_INICIAL} from '../constantes/Constantes';
import {GlobalState} from '../state/GlobalState';

const history = createBrowserHistory();
let initialState = {usuario: ESTADO_USUARIO_INICIAL};
        
/**
 * Se configura el store para almacenar el estado global de la aplicación.
 * Desde aquí se exportará para que pueda ser utilizado desde cualquier lugar de 
 * la aplicación
 */
export const store = createStore(
  connectRouter(history)(GlobalState),
  initialState,
  compose(applyMiddleware(routerMiddleware(history),thunk))
);


