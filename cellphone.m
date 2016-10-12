% CVX code for the M-step

load cellphone_data.mat

K = 3;

X = Design_Array_Calib;
Xstar = Choice_Matrix_Calib;

%I = size(Xstar,2);
I = 10;

J = 15;

p = length(X(1,:,J,I));


% randomly generate cluster membership 
q = rand(I,K);
for i = 1:I
    q(i,:) = q(i,:)/sum(q(i,:));
end



% M-step
gamma = zeros(p,K);
for k = 1:K
    
    cvx_begin
        variables y(p);
        expression loglik;
        
        loglik = 0;
        for i = 1:I
            for j = 1:J
                Xij = X(:,:,j,i);
                Xijstar = X(Xstar(j,i),:,j,i);
                loglik = loglik + q(i,k)*(Xijstar*y-log_sum_exp(Xij*y));
            end
        end
        maximize(loglik)
    cvx_end 
    
    gamma(:,k) = y;
end

omega = sum(q,1);
omega = omega/sum(omega);

% E-step

for i = 1:I
    for k = 1:K
        q(i,k) = omega(k);
        for j = 1:J
            Xij = X(:,:,j,i);
            Xijstar = X(Xstar(j,i),:,j,i);
            q(i,k) = q(i,k)*exp(Xijstar*gamma(:,k)-log_sum_exp(Xij*gamma(:,k)));
        end
    end
    q(i,:) = q(i,:)/sum(q(i,:));
end

    
