/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author mambrois
 */
public class BinOp extends Instr {
    enum operateur{Add,Sub,Mult,Div};
    
    operateur op;
    
    public BinOp(operateur oper){
        op = oper;
    }
    
    public void exec_instr(Config cf){

        switch(op){
            case Add:
            case Sub:
            case Mult:
            case Div:
                PairV pair = (PairV)cf.get_value();
                IntV valeur = new IntV(operArith((IntV) pair.getFstValue(),(IntV)pair.getSndValue()));
                cf.set_value(valeur);
                cf.get_code().pop();
                break;
        }
    }
    
    public int operArith(IntV a, IntV b){
        switch(op){
            case Add:
                return a.get_int() + b.get_int();
            case Sub:
                return a.get_int() - b.get_int();
            case Mult:
                return a.get_int() * b.get_int();
            case Div:
                return a.get_int() / b.get_int();
                
            default: 
                return 0;
        }
    }
}